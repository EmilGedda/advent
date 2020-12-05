# Advent of Code 2020

<p align="center">
  <a href="https://emilgedda.github.io/Advent-of-Code-2020/gold.svg" alt="Gold stars">
    <img src="https://emilgedda.github.io/Advent-of-Code-2020/gold.svg">
  </a>
  <a href="https://emilgedda.github.io/Advent-of-Code-2020/silver.svg" alt="Silver stars">
    <img src="https://emilgedda.github.io/Advent-of-Code-2020/silver.svg">
  </a>
  <a href="https://github.com/EmilGedda/Advent-of-Code-2020/workflows/Tests/badge.svg" alt="Tests">
    <img src="https://github.com/EmilGedda/Advent-of-Code-2020/workflows/Tests/badge.svg">
  </a>
</p>

## What

These are my solutions to Advent of Code 2020.
Also comes with a small library for interfacing with the website with features
such as:
 * Fetching input
 * Caching input to disk
 * Type infered parsing
 * Printing leaderboards and user progress
 * Test suite for inputs and solutions
 * Generating badges from progress

The daily solutions can be found in [Advent.Solutions](https://github.com/EmilGedda/Advent-of-Code-2020/tree/master/src/Advent/Solution) inside `src`.

## How

To use the library features, a session token must be present. The session token
can be found by inspecting your cookies on
[adventofcode.com](https://adventofcode.com/). The library expects this token to be put
inside a file called `session-token.txt` which should reside in
`$XDG_CONFIG_HOME/AdventOfCode` (the environment variable `XDG_CONFIG_HOME`
falls back to `~/.config` if not defined). If the file can't be found when
the program is ran, it prints the expected location for it in the error message.

The project is built with [stack](https://haskellstack.org/).

###### To run the tests
```bash
$ stack test
Tests
  Test consistency
    No days lacking tests: OK
    No tests lacking days: OK
  Stars
    Day 1
      Input:               OK
      Part 1:              OK
      Part 2:              OK (0.17s)
    Day 2
      Input:               OK
      Part 1:              OK (0.05s)
      Part 2:              OK (0.05s)
    Day 3
      Input:               OK
      Part 1:              OK (0.05s)
      Part 2:              OK (0.05s)
    Day 4
      Input:               OK
      Part 1:              OK (0.05s)
      Part 2:              OK (0.06s)
    Day 5
      Input:               OK
      Part 1:              OK (0.03s)
      Part 2:              OK

All 17 tests passed (0.17s)
```

###### Show current user progress in short format
```bash
$ stack run
Silver  0
Gold    5
```


###### Creating badges
The bash script takes a color and a star count.
```bash
$ ./.github/badge.sh gold 5 > gold-badge.svg
$ ./.github/badge.sh silver 0 > silver-badge.svg
```

# License
*SPDX identifier: BSD-3-Clause*

Copyright Emil Gedda (c) 2020

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above
  copyright notice, this list of conditions and the following
  disclaimer in the documentation and/or other materials provided
  with the distribution.

* Neither the name of Author name here nor the names of other
  contributors may be used to endorse or promote products derived
  from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

