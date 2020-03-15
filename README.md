katarchy
=====

A roguelike game about a self-sufficient colony that attempts to survive in a post-apocalyptic world against constant raids of mechanical units from an unknown origin.

It is inspired by the gameplay of _Plants vs Zombies_ mixed with the design ideas of _Slay the Spire_. In a colony phase the player will be able to react to random events, modify their defending mechs, take logistics decisions and plan a defense for the next raid by deploying their mechs. The raid phase will happen automatically without user input and, after that, they will be back to the next colony phase. This will continue until the colony is fully razed or until the player reach the yet-to-be-defined final confrontation against their enemies.

This repository covers an erlang server where all the logic takes place. Clients can use the exposed API to advance in the game and provide a visualization of what is taken place.

Running the server
-----

The code can be built by running the following command:

    $ make release

After that, the server can be started by running:

    $ ./_build/default/rel/katarchy/bin/katarchy console

This will start a HTTP server on port 8080. A basic functionality can be tested by sending a POST request to it, such as:

    $ curl -v -d '[{"id":"test_mech", "position":{"x":0, "y":0}, "speed":1}]' http://localhost:8080/siege

That request should return a `200 OK` and you could trace all the mech movements by looking at the `turns` array.

The server can be stopped by pressing `^G` and then inputting the `q` (quit) command.

Tests
-----

The project follows a test-driven development, aiming for a 100% code coverage. This can be confirmed with:

    $ make test

Furthermore, to help finding programming errors the output of the `xref` and `dialyzer` tools should also be clean. This can verified with:

    $ make analysis

Contributing
-----

The project is open to contributions and ideas of any kind. We are making rules as we go and nothing is set in stone.
