# VPL - a Visual Programming Language
VPL is a language initially developed at VandyHacks 2019, aiming to be
a _LOGO_ like language for the 21st century.

```haskell
-- Go forward: ↑
-- Clockwise, anti-clockwise rotation: ↻ ↺
-- Pen up, pen down: ○ ●

right = [ ↻ 90 ]
left  = [ ↺ 90 ]
-- Draw a square
□ n = [ loop 3 [↑ n, right], ↑ n, right]
-- Draw a triangle
△ n = [ ↻ 30, ↑ n, ↻ 120, ↑ n, ↻ 120, ↑ n, right ]
main = [ ●, △ 60, □ 60, right, □ 70, right, □ 80, ↑ 150,
         loop 100 [↑ 50, ↻ 59]]
```
