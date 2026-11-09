#ifndef PSK_REPORTER_HPP_
#define PSK_REPORTER_HPP_

#include <QObject>
#include <QDateTime>
#include <QString>
#include "Radio.hpp"
#include "PSKReporterIPFIX.hpp"
#include "pimpl_h.hpp"

class QString;
class Configuration;

class PSKReporter final
  : public QObject
{
  Q_OBJECT

public:
  struct Options
  {
    bool use_tcpip;
    QString eclipse_file_path;
    QString program_info;
    QString destination_host {"report.pskreporter.info"};
    quint16 destination_port {4739};
  };

  // "Log all": in LOG_ALL_CONSECUTIVE_SLOTS consecutive timeslots out of
  // every LOG_ALL_INTERVAL_SLOTS, every decoded message is reported, not only
  // those with a grid or CQ: free text and telemetry too, with no sender
  // call when the message names none. The repeat-spot filter is bypassed. The timeslot
  // number is <unix time of the slot start> / <timeslot length>; a slot is a
  // log-all slot when that number modulo the interval is below the
  // consecutive count. With FT8's 15 s slots that is one minute in three.
  static constexpr int LOG_ALL_INTERVAL_SLOTS {12};
  static constexpr int LOG_ALL_CONSECUTIVE_SLOTS {4};
  static_assert (LOG_ALL_CONSECUTIVE_SLOTS >= 0
                 && LOG_ALL_CONSECUTIVE_SLOTS <= LOG_ALL_INTERVAL_SLOTS,
                 "log-all slots must fit within the interval");

  // True when the timeslot starting at slotStart is a log-all slot. Slot
  // starts given to the whole second (FT4's 7.5 s slots) are rounded to
  // their slot.
  static bool isLogAllSlot (QDateTime const& slotStart, double periodSeconds);

  explicit PSKReporter (Configuration const *, QString const& program_info);
  explicit PSKReporter (Options const& options);
  ~PSKReporter ();

  void reconnect ();

  void setLocalStation (QString const& call, QString const& grid, QString const& antenna, QString const& rigInformation);

  // Returns false if accepting the spot required dropping an older pending spot.
  // extras carries what the decoder measured beyond the decoded text.
  // logAll queues the spot even when the call was spotted recently, or is
  // empty; it still counts as the call's latest spot for the repeat-spot
  // filter.
  bool addRemoteStation (QString const& call, QString const& grid, Radio::Frequency freq, QString const& mode, int snr, QDateTime spotTime
                         , PSKReporterIPFIX::SpotExtras const& extras = {}, bool logAll = false);

  //
  // Flush any pending spots to PSK Reporter
  //
  void sendReport (bool last = false);

  //
  // True if current time falls withing a +/- window of a solar eclipse for HamSCI use
  bool eclipse_active(QDateTime now);

  Q_SIGNAL void errorOccurred (QString const& reason);

private:
  class impl;
  pimpl<impl> m_;
};

#endif
