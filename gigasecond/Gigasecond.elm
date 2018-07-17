module Gigasecond exposing (add)

import Date
import Time

gigasec : Time.Time
gigasec = 10 ^ 12


add : Date.Date -> Date.Date
add date = 
    (Date.toTime >> (+) gigasec >> Date.fromTime) date