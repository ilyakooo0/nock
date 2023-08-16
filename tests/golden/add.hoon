!=
=> 
|%
++  dec
  ~/  %dec
  ::    unsigned decrement by one.
  |=  a=@
  ~_  leaf+"decrement-underflow"
  ?<  =(0 a)
  =+  b=0
  ::  decremented integer
  |-  ^-  @
  ?:  =(a +(b))  b
  $(b +(b))
++  add
  ~/  %add
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
%+  add
1.000  1.000
