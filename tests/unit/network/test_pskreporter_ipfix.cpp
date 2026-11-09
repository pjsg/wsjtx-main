#include <QtTest>

#include "Network/PSKReporterIPFIX.hpp"

namespace
{
  quint16 u16 (QByteArray const& bytes, int offset)
  {
    return (static_cast<quint8> (bytes[offset]) << 8)
      | static_cast<quint8> (bytes[offset + 1]);
  }

  quint32 u32 (QByteArray const& bytes, int offset)
  {
    return (static_cast<quint32> (static_cast<quint8> (bytes[offset])) << 24)
      | (static_cast<quint32> (static_cast<quint8> (bytes[offset + 1])) << 16)
      | (static_cast<quint32> (static_cast<quint8> (bytes[offset + 2])) << 8)
      | static_cast<quint8> (bytes[offset + 3]);
  }

  QList<quint16> setIds (QByteArray const& payload)
  {
    QList<quint16> ids;
    int offset = 16;
    while (offset + 4 <= payload.size ())
      {
        auto const id = u16 (payload, offset);
        auto const length = u16 (payload, offset + 2);
        ids.append (id);
        if (length < 4)
          {
            break;
          }
        offset += length;
      }
    return ids;
  }

  // Offset of the first set with this ID, or -1.
  int setOffset (QByteArray const& payload, quint16 set_id)
  {
    int offset = 16;
    while (offset + 4 <= payload.size ())
      {
        auto const length = u16 (payload, offset + 2);
        if (u16 (payload, offset) == set_id) return offset;
        if (length < 4) break;
        offset += length;
      }
    return -1;
  }

  // Skips one record field; a length of 0xffff in the template means a
  // length-prefixed field (one-byte prefix in these records).
  int skipField (QByteArray const& payload, int offset, quint16 length)
  {
    return 0xffff == length ? offset + 1 + static_cast<quint8> (payload[offset]) : offset + length;
  }

  PSKReporterIPFIX::Receiver receiver ()
  {
    return {"K1ABC", "FN20", "WSJT-X test", "N/A", "N/A"};
  }

  PSKReporterIPFIX::Spot spot (int index)
  {
    return {
      QString {"K%1ABC"}.arg (index),
      "FN21",
      -10,
      14074000u + static_cast<quint64> (index),
      "FT8",
      QDateTime {{2026, 5, 6}, {12, 0, 0}, Qt::UTC},
      {}
    };
  }

  QList<PSKReporterIPFIX::Packet> buildUdpPackets (PSKReporterIPFIX::Receiver const& rx
                                                   , QList<PSKReporterIPFIX::Spot> const& spots
                                                   , bool include_descriptors
                                                   , quint32 sequence_number
                                                   , quint32 observation_id
                                                   , quint32 export_time)
  {
    return PSKReporterIPFIX::buildPackets (
      rx, spots, include_descriptors, sequence_number, observation_id, export_time
      , PSKReporterIPFIX::maxUdpIpfixPayloadBytes ());
  }

  QList<PSKReporterIPFIX::Packet> buildTcpPackets (PSKReporterIPFIX::Receiver const& rx
                                                   , QList<PSKReporterIPFIX::Spot> const& spots
                                                   , bool include_descriptors
                                                   , quint32 sequence_number
                                                   , quint32 observation_id
                                                   , quint32 export_time)
  {
    return PSKReporterIPFIX::buildPackets (
      rx, spots, include_descriptors, sequence_number, observation_id, export_time
      , PSKReporterIPFIX::maxTcpIpfixPayloadBytes ());
  }
}

class TestPSKReporterIPFIX final
  : public QObject
{
  Q_OBJECT

private slots:
  void splitsManySpotsIntoBoundedUdpPayloads ()
  {
    QList<PSKReporterIPFIX::Spot> spots;
    for (int i = 0; i != 200; ++i)
      {
        spots.append (spot (i));
      }

    auto const packets = buildUdpPackets (receiver (), spots, true, 17, 23, 29);

    QVERIFY (packets.size () > 1);
    int spot_count = 0;
    for (auto const& packet : packets)
      {
        QVERIFY2 (packet.payload.size () <= PSKReporterIPFIX::maxUdpIpfixPayloadBytes (), qPrintable (QString::number (packet.payload.size ())));
        spot_count += packet.spot_count;
      }
    QCOMPARE (spot_count, spots.size ());
  }

  void truncatesLongFieldsAndKeepsPacketsBounded ()
  {
    auto long_text = QString (300, QChar {0x00f8});
    PSKReporterIPFIX::Receiver rx {long_text, long_text, long_text, long_text, long_text};
    QList<PSKReporterIPFIX::Spot> spots {
      {long_text, long_text, -1, 50313000u, long_text, QDateTime {{2026, 5, 6}, {12, 0, 0}, Qt::UTC}, {}}
    };

    auto const packets = buildUdpPackets (rx, spots, true, 1, 2, 3);

    QCOMPARE (packets.size (), 1);
    QCOMPARE (packets.front ().spot_count, 1);
    QVERIFY (packets.front ().payload.size () <= PSKReporterIPFIX::maxUdpIpfixPayloadBytes ());
  }

  void receiverOptionsTemplateUsesOneScopeField ()
  {
    auto const packets = buildUdpPackets (receiver (), {}, true, 1, 2, 3);
    auto const payload = packets.front ().payload;

    int offset = 16;
    QCOMPARE (u16 (payload, offset), quint16 (2u));
    offset += u16 (payload, offset + 2);
    QCOMPARE (u16 (payload, offset), quint16 (3u));
    QCOMPARE (u16 (payload, offset + 8), quint16 (1u));
  }

  void sequenceNumberAdvancesBySpotRecords ()
  {
    QList<PSKReporterIPFIX::Spot> spots;
    for (int i = 0; i != 200; ++i)
      {
        spots.append (spot (i));
      }

    auto const packets = buildUdpPackets (receiver (), spots, true, 100, 23, 29);

    quint32 expected = 100;
    for (auto const& packet : packets)
      {
        QCOMPARE (u32 (packet.payload, 8), expected);
        expected += packet.spot_count;
      }
    QCOMPARE (expected, quint32 (100 + spots.size ()));
  }

  void descriptorOnlyPacketDoesNotAdvanceSequence ()
  {
    auto const packets = buildUdpPackets (receiver (), {}, true, 44, 23, 29);

    QCOMPARE (packets.size (), 1);
    QCOMPARE (packets.front ().spot_count, 0);
    QCOMPARE (u32 (packets.front ().payload, 8), quint32 (44));
  }

  void splitPacketsOnlyCarryDescriptorsInFirstPacket ()
  {
    QList<PSKReporterIPFIX::Spot> spots;
    for (int i = 0; i != 200; ++i)
      {
        spots.append (spot (i));
      }

    auto const packets = buildUdpPackets (receiver (), spots, true, 1, 2, 3);

    QVERIFY (packets.size () > 1);
    QVERIFY (setIds (packets.front ().payload).contains (quint16 (2u)));
    QVERIFY (setIds (packets.front ().payload).contains (quint16 (3u)));
    for (int i = 1; i != packets.size (); ++i)
      {
        QVERIFY (!setIds (packets[i].payload).contains (quint16 (2u)));
        QVERIFY (!setIds (packets[i].payload).contains (quint16 (3u)));
      }
  }

  void everyPacketCarriesReceiverAndSenderSetsWhenSpotsSplit ()
  {
    QList<PSKReporterIPFIX::Spot> spots;
    for (int i = 0; i != 200; ++i)
      {
        spots.append (spot (i));
      }

    auto const packets = buildUdpPackets (receiver (), spots, true, 1, 2, 3);

    QVERIFY (packets.size () > 1);
    for (auto const& packet : packets)
      {
        auto const ids = setIds (packet.payload);
        QVERIFY (ids.contains (quint16 (0x50e2)));
        QVERIFY (ids.contains (quint16 (0x50e5)));
      }
  }

  void senderTemplateDeclaresDecoderMeasurements ()
  {
    auto const packets = buildUdpPackets (receiver (), {}, true, 1, 2, 3);
    auto const payload = packets.front ().payload;

    auto const offset = setOffset (payload, 2u);
    QVERIFY (offset >= 0);
    QCOMPARE (u16 (payload, offset + 4), quint16 (0x50e5));
    QCOMPARE (u16 (payload, offset + 6), quint16 (10u));
    // The eighth field specifier follows six enterprise fields (8 bytes each)
    // and dateTimeSeconds (4 bytes).
    auto field = offset + 8 + 6 * 8 + 4;
    for (auto const& expected : {qMakePair (14u, 0xffffu), qMakePair (15u, 2u), qMakePair (16u, 2u)})
      {
        QCOMPARE (u16 (payload, field), quint16 (0x8000 + expected.first));
        QCOMPARE (u16 (payload, field + 2), quint16 (expected.second));
        QCOMPARE (u32 (payload, field + 4), quint32 (30351u));
        field += 8;
      }
    QCOMPARE (field, offset + u16 (payload, offset + 2));
  }

  void spotRecordCarriesDecoderMeasurements_data ()
  {
    QTest::addColumn<QByteArray> ("bits");
    QTest::addColumn<QByteArray> ("expected");
    QTest::addColumn<qint16> ("deltaT");
    QTest::addColumn<quint16> ("fraction");
    auto const bits = QByteArray::fromHex ("0123456789ABCDEF0120");
    QTest::newRow ("known") << bits << bits << qint16 (-1234) << quint16 (0x8000);
    QTest::newRow ("unknown") << QByteArray {} << QByteArray {} << PSKReporterIPFIX::DELTA_T_UNKNOWN << quint16 (0);
    QTest::newRow ("wrong size dropped") << QByteArray::fromHex ("0123") << QByteArray {} << qint16 (5000) << quint16 (1);
  }

  void spotRecordCarriesDecoderMeasurements ()
  {
    QFETCH (QByteArray, bits);
    QFETCH (QByteArray, expected);
    QFETCH (qint16, deltaT);
    QFETCH (quint16, fraction);
    auto s = spot (1);
    s.extras.payload77 = bits;
    s.extras.delta_t = deltaT;
    s.extras.fractional_frequency = fraction;

    auto const packets = buildUdpPackets (receiver (), {s}, false, 1, 2, 3);
    QCOMPARE (packets.size (), 1);
    auto const payload = packets.front ().payload;
    auto const set = setOffset (payload, 0x50e5);
    QVERIFY (set >= 0);

    // Field lengths of template 0x50e5 before messageBits, in order.
    int offset = set + 4;
    for (quint16 length : {quint16 (0xffff), quint16 (5u), quint16 (1u), quint16 (0xffff)
                           , quint16 (0xffff), quint16 (1u), quint16 (4u)})
      {
        offset = skipField (payload, offset, length);
      }
    QCOMPARE (static_cast<int> (static_cast<quint8> (payload[offset])), expected.size ());
    QCOMPARE (payload.mid (offset + 1, expected.size ()), expected);
    offset += 1 + expected.size ();
    QCOMPARE (static_cast<qint16> (u16 (payload, offset)), deltaT);
    QCOMPARE (u16 (payload, offset + 2), fraction);
    // Only alignment padding follows the record.
    auto const end = set + u16 (payload, set + 2);
    QVERIFY (end - (offset + 4) < 4);
  }

  void tcpCanUseLargerIpfixPayloads ()
  {
    QList<PSKReporterIPFIX::Spot> spots;
    for (int i = 0; i != 200; ++i)
      {
        spots.append (spot (i));
      }

    auto const udp_packets = buildUdpPackets (receiver (), spots, true, 1, 2, 3);
    auto const tcp_packets = buildTcpPackets (receiver (), spots, true, 1, 2, 3);

    QVERIFY (udp_packets.size () > 1);
    QCOMPARE (tcp_packets.size (), 1);
    QVERIFY (tcp_packets.front ().payload.size () > PSKReporterIPFIX::maxUdpIpfixPayloadBytes ());
    QVERIFY (tcp_packets.front ().payload.size () <= PSKReporterIPFIX::maxTcpIpfixPayloadBytes ());
    QCOMPARE (tcp_packets.front ().spot_count, spots.size ());
  }
};

QTEST_MAIN (TestPSKReporterIPFIX)

#include "test_pskreporter_ipfix.moc"
