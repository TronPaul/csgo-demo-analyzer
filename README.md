# csgo-demo-reader

A Clojure library designed to read Counter-Strike: Global Offensive demos

## Usage

Call the `read-demo` method with the demo filename and handler functions for different types of demo events. For example:

    (use 'csgo_demo_reader.core)
    (read-demo "match730_003123845571123609706_1493126170_125.dem" {:demo-header println :packet-cmds (reduce #(assoc %1 %2 println) {} (keys commands))})

This will print the demo header and every packet in the demo.

## License

Copyright © 2016 Mark McGuire

Counter-Strike: Global Offensive proto files are Copyright © 2014, Valve Corporation, All rights reserved.

Distributed under the MIT License
