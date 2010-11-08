package org.zmpp.glulx.swing

import java.util.logging._

class ZmppLogFormatter extends Formatter {
  def format(record: LogRecord): String = {
    "[%s-%s] %s\n".format(record.getLoggerName, record.getLevel.getName, record.getMessage)
  }
}

