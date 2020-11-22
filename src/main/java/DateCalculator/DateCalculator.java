package DateCalculator;

import org.joda.time.DateTime;
import org.joda.time.Days;
import java.util.Date;

public class DateCalculator {
   public int daysBetween(Date start, Date end) {
     Days d = Days.daysBetween(new DateTime(start.getTime()),
     new DateTime(end.getTime()));
     return d.getDays();
      }
    }
