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
--

%-  dec  8
