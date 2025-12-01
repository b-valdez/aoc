A repository for solving the Advent of Code problems.

# Structure

- `solutions` contains the solutions for the problems, separated by year and day.
- `bin` contains an executable for quickly scaffolding a new folder for a new day.
- `lib` contains `aoc_std`, a library extending and bundling `core`, `angstrom` and `iter` for easier importing.
- `test` contains some tests for `lib`.

The inputs are omitted from this repository because [the author doesn't want them to be distributed](https://adventofcode.com/about).

# TODOS

I would not expect any solution from 2023 to work. They have not been tested with current versions of packages and there have been some changes in the project structure since. The solutions should be adapted and tested.

There is a weird parsing error in 2024/16 that I should fix before it becomes a problem in 2025.

2024/21 doesn't seem to resolve in a reasonable timeframe. Need to investigate.

Parallel_iter was an experiment. I should try and see if the heavy parallelization is worth it.

Investigate if core can be dropped as a dependency.