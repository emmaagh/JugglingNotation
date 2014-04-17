module Siteswap

    // Returns the index i is sent to after a siteswap on j and k
    let siteswap j k i =
        if      i = j then k
        else if i = k then j
        else               i