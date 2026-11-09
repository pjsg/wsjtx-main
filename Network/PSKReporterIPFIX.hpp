#ifndef PSK_REPORTER_IPFIX_HPP_
#define PSK_REPORTER_IPFIX_HPP_

#include <QByteArray>
#include <QDateTime>
#include <QList>
#include <QString>

#include "Radio.hpp"

namespace PSKReporterIPFIX
{
  int maxUdpIpfixPayloadBytes ();
  int maxTcpIpfixPayloadBytes ();

  struct Receiver
  {
    QString callsign;
    QString locator;
    QString program_info;
    QString antenna;
    QString rig_information;
  };

  // deltaT (30351.15) value meaning unknown.
  constexpr qint16 DELTA_T_UNKNOWN = -0x8000;

  // Measurements beyond what the decoded text gives.
  struct SpotExtras
  {
    // Empty, or the message bits the spot was decoded from, first bit most
    // significant, padded with zero bits to whole bytes: 77 bits in 10 bytes
    // (FT8, FT4, Q65, MSK144, FST4), 72 in 9 (JT4, JT9, JT65), 50 in 7
    // (FST4W). The mode says which. At most 16 bytes are sent.
    QByteArray message_bits;
    // Message start relative to the start of the transmission period, in
    // 100 us units.
    qint16 delta_t {DELTA_T_UNKNOWN};
    // Fractional part of the frequency, in 1/65536 Hz. Zero when the
    // frequency is only known to the nearest Hz.
    quint16 fractional_frequency {0};
  };

  struct Spot
  {
    QString callsign;
    QString locator;
    int snr;
    Radio::Frequency frequency;
    QString mode;
    QDateTime time;
    SpotExtras extras;
  };

  struct Packet
  {
    QByteArray payload;
    int spot_count;
  };

  QList<Packet> buildPackets (Receiver const& receiver, QList<Spot> const& spots
                              , bool include_descriptors, quint32 sequence_number
                              , quint32 observation_id, quint32 export_time
                              , int max_payload_bytes);
}

#endif
