#include <QtTest>

#include <QHostAddress>
#include <QSet>
#include <QTcpServer>
#include <QTcpSocket>
#include <QUdpSocket>

#include "Network/PSKReporter.hpp"
#include "Network/PSKReporterIPFIX.hpp"

namespace
{
  quint16 u16(QByteArray const& bytes, int offset)
  {
    return (static_cast<quint8>(bytes[offset]) << 8)
      | static_cast<quint8>(bytes[offset + 1]);
  }

  QList<quint16> setIds(QByteArray const& payload)
  {
    QList<quint16> ids;
    int offset = 16;
    while (offset + 4 <= payload.size()) {
      auto const length = u16(payload, offset + 2);
      ids.append(u16(payload, offset));
      if (length < 4) {
        break;
      }
      offset += length;
    }
    return ids;
  }

  void verifyReport(QByteArray const& payload, bool includeDescriptors)
  {
    QVERIFY(payload.size() >= 16);
    QCOMPARE(u16(payload, 0), quint16(10));
    QCOMPARE(u16(payload, 2), static_cast<quint16>(payload.size()));

    auto const ids = setIds(payload);
    QVERIFY(ids.contains(quint16(0x50e2)));
    QVERIFY(ids.contains(quint16(0x50e5)));
    QCOMPARE(ids.contains(quint16(2)), includeDescriptors);
    QCOMPARE(ids.contains(quint16(3)), includeDescriptors);
  }

  PSKReporter::Options options(bool useTcpIp, quint16 port)
  {
    return {useTcpIp, QString {}, "MAP65 transport test", "127.0.0.1", port};
  }

  bool addSpots(PSKReporter& reporter, QString const& prefix,
                QStringList& callsigns)
  {
    for (int i = 0; i < 2048; ++i) {
      auto suffixIndex = i;
      QString suffix;
      for (int j = 0; j < 4; ++j) {
        suffix.prepend(QChar {
          static_cast<ushort>('A' + (suffixIndex % 26))});
        suffixIndex /= 26;
      }
      auto const callsign = prefix + QString::number((i / 26) % 10) + suffix;
      if (!reporter.addRemoteStation(
            callsign, "FN20", 50313000u, "Q65-60A", -12,
            QDateTime {{2026, 8, 17}, {12, 0, 0}, Qt::UTC})) {
        return false;
      }
      callsigns.append(callsign);
    }
    return true;
  }

  void accountForPayload(QByteArray const& payload, QSet<QString>& remaining)
  {
    for (auto const& callsign : remaining.values()) {
      if (payload.contains(callsign.toUtf8())) {
        remaining.remove(callsign);
      }
    }
  }

  QList<QByteArray> takeCompleteFrames(QByteArray& stream)
  {
    QList<QByteArray> frames;
    while (stream.size() >= 4) {
      auto const length = u16(stream, 2);
      if (length < 16 || stream.size() < length) {
        break;
      }
      frames.append(stream.left(length));
      stream.remove(0, length);
    }
    return frames;
  }
}

class TestPSKReporterTransport final : public QObject
{
  Q_OBJECT

private slots:
  void logAllSlots_data()
  {
    QTest::addColumn<QDateTime>("slotStart");
    QTest::addColumn<double>("period");
    QTest::addColumn<bool>("logAll");
    auto const base = QDateTime::fromSecsSinceEpoch(0, Qt::UTC);
    // FT8: slots of 15 s, so slots 0..3 of every 12 (one minute in three).
    QTest::newRow("FT8 slot 0") << base << 15.0 << true;
    QTest::newRow("FT8 slot 3") << base.addSecs(45) << 15.0 << true;
    QTest::newRow("FT8 slot 4") << base.addSecs(60) << 15.0 << false;
    QTest::newRow("FT8 slot 11") << base.addSecs(165) << 15.0 << false;
    QTest::newRow("FT8 slot 12") << base.addSecs(180) << 15.0 << true;
    QTest::newRow("FT8 now") << QDateTime {{2026, 11, 9}, {12, 0, 45}, Qt::UTC} << 15.0
                             << ((QDateTime {{2026, 11, 9}, {12, 0, 45}, Qt::UTC}.toSecsSinceEpoch() / 15) % 12 < 4);
    // FT4: slots of 7.5 s, whose spot times are rounded down to the second.
    QTest::newRow("FT4 slot 1 at 7 s") << base.addSecs(7) << 7.5 << true;
    QTest::newRow("FT4 slot 3 at 22 s") << base.addSecs(22) << 7.5 << true;
    QTest::newRow("FT4 slot 4 at 30 s") << base.addSecs(30) << 7.5 << false;
    QTest::newRow("FT4 slot 12 at 90 s") << base.addSecs(90) << 7.5 << true;
    QTest::newRow("no period") << base << 0.0 << false;
  }

  void logAllSlots()
  {
    QFETCH(QDateTime, slotStart);
    QFETCH(double, period);
    QFETCH(bool, logAll);
    QCOMPARE(PSKReporter::isLogAllSlot(slotStart, period), logAll);
  }

  void logAllBypassesRepeatFilterAndUpdatesIt()
  {
    QUdpSocket receiver;
    QVERIFY(receiver.bind(QHostAddress {QHostAddress::LocalHost}, 0));
    PSKReporter reporter {options(false, receiver.localPort())};
    reporter.setLocalStation("K1ABC", "FN21", "N/A", "N/A");
    QDateTime const time {{2026, 11, 9}, {12, 0, 0}, Qt::UTC};
    auto spot = [&](QString const& call, bool logAll) {
      reporter.addRemoteStation(call, "FN20", 14074000u, "FT8", -10, time, {}, logAll);
    };

    spot("K1RPT", false);
    spot("K1RPT", false);           // a repeat within the cache timeout: dropped
    spot("K1RPT", true);            // log all: sent anyway
    spot("K2LOG", true);
    spot("K2LOG", false);           // the log-all spot counts as K2LOG's latest
    reporter.sendReport();

    QByteArray received;
    QTRY_VERIFY_WITH_TIMEOUT(([&] {
      while (receiver.hasPendingDatagrams()) {
        QByteArray payload;
        payload.resize(static_cast<int>(receiver.pendingDatagramSize()));
        receiver.readDatagram(payload.data(), payload.size());
        received += payload;
      }
      return received.contains("K2LOG");
    }()), 5000);
    QCOMPARE(received.count("K1RPT"), 2);
    QCOMPARE(received.count("K2LOG"), 1);
    reporter.sendReport(true);
  }

  void logAllSendsMessagesNamingNoSender()
  {
    QUdpSocket receiver;
    QVERIFY(receiver.bind(QHostAddress {QHostAddress::LocalHost}, 0));
    PSKReporter reporter {options(false, receiver.localPort())};
    reporter.setLocalStation("K1ABC", "FN21", "N/A", "N/A");
    QDateTime const time {{2026, 11, 9}, {12, 0, 0}, Qt::UTC};
    // Two telemetry messages: no call, no grid, told apart by their bits.
    PSKReporterIPFIX::SpotExtras first;
    first.message_bits = QByteArray::fromHex("123456789ABCDEF01228");
    PSKReporterIPFIX::SpotExtras second;
    second.message_bits = QByteArray::fromHex("FEDCBA987654321012A8");
    reporter.addRemoteStation("", "", 14074000u, "FT8", -10, time, first, true);
    reporter.addRemoteStation("", "", 14074100u, "FT8", -12, time, second, true);
    reporter.sendReport();

    QByteArray received;
    QTRY_VERIFY_WITH_TIMEOUT(([&] {
      while (receiver.hasPendingDatagrams()) {
        QByteArray payload;
        payload.resize(static_cast<int>(receiver.pendingDatagramSize()));
        receiver.readDatagram(payload.data(), payload.size());
        received += payload;
      }
      return received.contains(second.message_bits);
    }()), 5000);
    QVERIFY(received.contains(first.message_bits));
    reporter.sendReport(true);
  }

  void sendsAllIpfixReportsOverUdp()
  {
    QUdpSocket receiver;
    QVERIFY(receiver.bind(QHostAddress {QHostAddress::LocalHost}, 0));
    // sendReport writes the maximum queue synchronously, before this test
    // begins draining received datagrams.
    receiver.setSocketOption(QAbstractSocket::ReceiveBufferSizeSocketOption,
                             256 * 1024);

    PSKReporter reporter {options(false, receiver.localPort())};
    reporter.setLocalStation("K1ABC", "FN21", "N/A", "N/A (MAP65)");
    QStringList callsigns;
    QVERIFY(addSpots(reporter, "UA", callsigns));
    QSet<QString> remaining {callsigns.begin(), callsigns.end()};

    reporter.sendReport();
    QList<QByteArray> reports;
    QTRY_VERIFY_WITH_TIMEOUT(([&] {
      while (receiver.hasPendingDatagrams()) {
        QByteArray payload;
        payload.resize(static_cast<int>(receiver.pendingDatagramSize()));
        if (receiver.readDatagram(payload.data(), payload.size())
            != static_cast<qint64>(payload.size())) {
          return false;
        }
        reports.append(payload);
        accountForPayload(payload, remaining);
      }
      return reports.size() > 1 && remaining.isEmpty();
    }()), 5000);

    QVERIFY(reports.size() > 1);
    for (int i = 0; i < reports.size(); ++i) {
      verifyReport(reports.at(i), i == 0);
    }
    reporter.sendReport(true);
  }

  void sendsAllIpfixReportsOverTcp()
  {
    QTcpServer server;
    QVERIFY(server.listen(QHostAddress {QHostAddress::LocalHost}, 0));

    PSKReporter reporter {options(true, server.serverPort())};
    reporter.setLocalStation("K1ABC", "FN21", "N/A", "N/A (MAP65)");
    QTRY_VERIFY_WITH_TIMEOUT(server.hasPendingConnections(), 3000);
    auto *peer = server.nextPendingConnection();
    QVERIFY(peer);

    QStringList callsigns;
    QVERIFY(addSpots(reporter, "TA", callsigns));
    QSet<QString> remaining {callsigns.begin(), callsigns.end()};
    QByteArray stream;
    QList<QByteArray> reports;
    reporter.sendReport();

    QTRY_VERIFY_WITH_TIMEOUT(([&] {
      stream.append(peer->readAll());
      auto const newFrames = takeCompleteFrames(stream);
      for (auto const& frame : newFrames) {
        reports.append(frame);
        accountForPayload(frame, remaining);
      }
      return reports.size() > 1 && remaining.isEmpty();
    }()), 5000);

    QVERIFY(reports.size() > 1);
    for (int i = 0; i < reports.size(); ++i) {
      verifyReport(reports.at(i), i == 0);
    }
    reporter.sendReport(true);
  }
};

QTEST_MAIN(TestPSKReporterTransport)

#include "test_pskreporter_transport.moc"
