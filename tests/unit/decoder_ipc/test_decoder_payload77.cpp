// SPDX-License-Identifier: GPL-3.0-or-later
// jt9 under shared memory precedes each FT8 decode with a <DecodeEvidence>
// line: the measured message start and frequency, and the message bits when
// they were received as such.
#include <QByteArray>
#include <QFile>
#include <QProcess>
#include <QTemporaryDir>
#include <QUuid>
#include <QtEndian>
#include <QtTest>

#include <cstring>
#include <memory>
#include <optional>

#include "wsjtx_config.h"
#include "commons.h"
#include "DecoderIpc.hpp"
#include "lib/NativeSharedMemory.hpp"

extern "C" {
  void genft8_(char* msg, int* i3, int* n3, char* msgsent, char ft8msgbits[],
               int itone[], fortran_charlen_t, fortran_charlen_t);
  void genft4_(char* msg, int* ichk, char* msgsent, char ft4msgbits[], int itone[],
               fortran_charlen_t, fortran_charlen_t);
}

namespace
{
  // 16-bit mono samples of a WAV file's data chunk.
  QByteArray wavSamples (QString const& path)
  {
    QFile file {path};
    if (!file.open (QIODevice::ReadOnly)) return {};
    auto const bytes = file.readAll ();
    int offset = 12;
    while (offset + 8 <= bytes.size ())
      {
        auto const id = bytes.mid (offset, 4);
        auto const size = qFromLittleEndian<quint32> (bytes.constData () + offset + 4);
        if ("data" == id) return bytes.mid (offset + 8, size);
        offset += 8 + size + (size & 1);
      }
    return {};
  }

  // FT4's scrambling sequence (lib/ft4/genft4.f90), which genft4's message
  // bits include and decoder evidence does not.
  constexpr char ft4Scramble[77] {
    0,1,0,0,1,0,1,0,0,1,0,1,1,1,1,0,1,0,0,0,1,0,0,1,1,0,1,1,0,
    1,0,0,1,0,1,1,0,0,0,0,1,0,0,0,1,0,1,0,0,1,1,1,1,0,0,1,0,1,
    0,1,0,1,0,1,1,0,1,1,1,1,1,0,0,0,1,0,1};

  // The bits genft8 or genft4 packs for a message, without FT4's scrambling,
  // in the 10-byte payload77 form.
  QByteArray packedPayload77 (QByteArray const& message, int mode)
  {
    char msg[37];
    char sent[37];
    char bits[77];
    int tones[103];
    std::memset (msg, ' ', sizeof msg);
    std::memcpy (msg, message.constData (), qMin<int> (message.size (), sizeof msg));
    if (8 == mode)
      {
        int i3 {0};
        int n3 {0};
        genft8_ (msg, &i3, &n3, sent, bits, tones, 37, 37);
      }
    else
      {
        int ichk {0};
        genft4_ (msg, &ichk, sent, bits, tones, 37, 37);
        for (int i = 0; i < 77; ++i) bits[i] = static_cast<char> (bits[i] ^ ft4Scramble[i]);
      }
    QByteArray payload (10, '\0');
    for (int i = 0; i < 77; ++i)
      {
        if (bits[i]) payload[i / 8] = static_cast<char> (payload[i / 8] | (0x80 >> (i % 8)));
      }
    return payload;
  }

  // A standard (i3 1 or 2) payload with its 15-bit grid field, bits 60..74
  // counting from 1, replaced.
  QByteArray withGrid15 (QByteArray payload, int g15)
  {
    for (int k = 0; k < 15; ++k)
      {
        auto const bit = 59 + k;
        auto const mask = static_cast<char> (0x80 >> (bit % 8));
        if (g15 & (1 << (14 - k))) payload[bit / 8] = static_cast<char> (payload[bit / 8] | mask);
        else payload[bit / 8] = static_cast<char> (payload[bit / 8] & ~mask);
      }
    return payload;
  }
}

class TestDecoderPayload77 final
  : public QObject
{
  Q_OBJECT

private slots:
  void decodesCarryTheirEvidence_data ()
  {
    QTest::addColumn<QString> ("sample");
    QTest::addColumn<int> ("mode");
    QTest::addColumn<bool> ("multithread");
    QTest::newRow ("ft8b") << QStringLiteral (FT8_SAMPLE_WAV) << 8 << false;
    QTest::newRow ("ft8bvar") << QStringLiteral (FT8_SAMPLE_WAV) << 8 << true;
    QTest::newRow ("ft4") << QStringLiteral (FT4_SAMPLE_WAV) << 5 << false;
  }

  void decodesCarryTheirEvidence ()
  {
    QFETCH (QString, sample);
    QFETCH (int, mode);
    QFETCH (bool, multithread);
    auto const samples = wavSamples (sample);
    QVERIFY (samples.size () >= 2 * 6 * RX_SAMPLE_RATE);

    auto const key = DecoderIpc::memoryName (QUuid::createUuid ().toString ());
    NativeSharedMemory memory;
    QVERIFY2 (memory.create (key.toStdString (), sizeof (shared_dec_data_t)), memory.errorString ().c_str ());
    auto * shared = static_cast<shared_dec_data_t *> (memory.data ());
    DecoderIpc::initialize (*shared);

    auto payload = std::make_unique<dec_data_t> ();
    auto const count = qMin<int> (samples.size () / 2, 15 * RX_SAMPLE_RATE);
    std::memcpy (payload->d2, samples.constData (), count * 2);
    auto& params = payload->params;
    params.nutc = 8 == mode ? 133430 : 2;
    params.nmode = mode;
    params.ntrperiod = 8 == mode ? 15 : 7;
    params.nzhsym = 50;
    params.newdat = true;
    params.nfa = 200;
    params.nfb = 4000;
    params.nfqso = 1500;
    params.nftx = 1500;
    params.ntol = 20;
    params.ndepth = 3;
    params.kin = count;
    std::memset (params.mycall, ' ', sizeof params.mycall);
    std::memset (params.hiscall, ' ', sizeof params.hiscall);
    std::memset (params.mybcall, ' ', sizeof params.mybcall);
    std::memset (params.hisbcall, ' ', sizeof params.hisbcall);
    std::memset (params.mygrid, ' ', sizeof params.mygrid);
    std::memset (params.hisgrid, ' ', sizeof params.hisgrid);
    if (multithread)
      {
        params.lmultift8 = true;
        params.nmt = 1;
        params.nft8cycles = 3;
        params.nft8rxfsens = 3;
        params.ncandthin = 100;
        params.nprepass = 4;
        params.nsdecatt = 1;
        params.ndecoderstart = 3;
      }
    else
      {
        params.lcommonft8b = true;
      }

    QTemporaryDir directory;
    QVERIFY (directory.isValid ());
    QProcess decoder;
    QByteArray output;
    connect (&decoder, &QProcess::readyReadStandardOutput, this, [&] {
        output += decoder.readAllStandardOutput ();
      });
    decoder.start (QString::fromLocal8Bit (JT9_TEST_EXECUTABLE),
                   {"-s", key, "--ipc-lock", directory.filePath ("worker.lock"),
                    "-a", directory.path (), "-t", directory.path ()});
    QVERIFY (decoder.waitForStarted ());
    QTRY_VERIFY_WITH_TIMEOUT (output.contains ("<DecoderReady>"), 5000);
    QVERIFY (DecoderIpc::publish (*shared, *payload, true, 7, {11, 12, 1, count}));
    QTRY_VERIFY_WITH_TIMEOUT (output.contains ("<DecodeFinished>"), 60000);
    QVERIFY (DecoderIpc::consume (*shared, 7));
    DecoderIpc::shutdown (*shared);
    QVERIFY (decoder.waitForFinished (5000));

    int decodes {0};
    int compared {0};
    std::optional<DecoderIpc::DecodeEvidence> pending;
    for (auto const& line : output.split ('\n'))
      {
        DecoderIpc::DecodeEvidence evidence;
        if (DecoderIpc::parseDecodeEvidence (line, &evidence))
          {
            QVERIFY2 (!pending, "two evidence lines in a row");
            pending = evidence;
            continue;
          }
        if (line.size () < 24 || line.mid (20, 3) != (8 == mode ? " ~ " : " + "))
          {
            QVERIFY2 (!pending, line.constData ());
            continue;
          }
        ++decodes;
        QVERIFY2 (pending, line.constData ());
        auto const dt = line.mid (10, 5).trimmed ().toDouble ();
        auto const freq = line.mid (15, 5).trimmed ().toInt ();
        auto const annotation = line.mid (24 + 37 + 1).trimmed ();
        auto const message = line.mid (24, 37).trimmed ();

        QVERIFY2 (pending->hasStart, line.constData ());
        // The measured start and frequency are refined from the decoder's
        // coarse estimates, which the displayed DT (relative to the nominal
        // start 0.5 s into the period, to 0.1 s) and frequency (to 1 Hz)
        // round further.
        QVERIFY2 (qAbs (pending->startSeconds - 0.5 - dt) <= 0.06, line.constData ());
        QVERIFY2 (qAbs (pending->frequencyHz - freq) <= 1.5, line.constData ());


        if ("a7" == annotation || "a8" == annotation)
          {
            QVERIFY2 (pending->payload77.isEmpty (), line.constData ());
          }
        else
          {
            QVERIFY2 (!pending->payload77.isEmpty (), line.constData ());
            if (!message.contains ('<') && !message.endsWith ('?'))
              {
                auto const expected = packedPayload77 (message, mode);
                // Text cannot say which of two encodings of RR73 was sent: the
                // RR73 code genft8 uses (g15 32403), or the grid square RR73
                // (g15 32373) that some senders, including two in this sample, use.
                auto const asGrid = message.endsWith (" RR73")
                  ? withGrid15 (expected, 32373) : QByteArray {};
                QVERIFY2 (pending->payload77 == expected || pending->payload77 == asGrid,
                          (message + ": " + pending->payload77.toHex () + " != " + expected.toHex ()).constData ());
                ++compared;
              }
          }
        pending.reset ();
      }
    QVERIFY2 (decodes >= 3, output.constData ());
    QVERIFY2 (compared >= 3, output.constData ());
  }
};

QTEST_GUILESS_MAIN (TestDecoderPayload77)

#include "test_decoder_payload77.moc"
