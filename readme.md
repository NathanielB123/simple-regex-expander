# Simple Regex Expander

I found myself wanting some tool to, given some Regex, return a list of
potential matching strings (i.e: to quickly see if there are false-positives).

After a bit of Google searching, I surprisingly couldn't find anything, so I
thought I would try to write it myself.

# Interesting Bits

- To actual expansion algorithm is basically just a DFS with iterative
  deepening. This keeps the memory usage from balooning while avoiding expanding
  the last repetition infinitely.
- Alternations ("|"s) can have a lot of possibilities, especially when using
  ranges like "a-z". To avoid different alternation choices from clogging up
  the output, they are expanded like repetitions, with the number of choices
  restricted to the depth.
- Handling concatenation correctly took a bit of care. At first, I planned on
  using " " as the concatenation operator, but this made handling of ordinary
  whitespace challenging (i.e: "(a )" would fail to parse as a regex expression
  would be expected after the space) and also meant regex like "(ab)(a)" would
  fail to parse. Instead, the current solution I am using is to interpret
  concatenation as a postfix operator with the parser just being regex. This
  avoids infinite recursion as to reach the point where postfix operator is
  being parsed, the term must have been consumed beforehand.
