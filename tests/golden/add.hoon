!=
=> 
|%
++  foo  1
++  baz  |=  x=@  x
++  dec
|=  a=@
=/  b  0
|-  ^-  @
?.  =(a +(b))  $(b +(b))  b
++  bar  2
++  add
  :: ~/  %add
  ::    unsigned addition
  ::
  ::  a: augend
  ::  b: addend
  |=  [a=@ b=@]
  ::  sum
  ^-  @
  ?:  =(0 a)  b
  $(a (dec a), b +(b))
--
(add 3 5) 
