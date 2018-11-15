This is a web-based interface with shell. I have it deployed on my Raspberry Pi to achieve a uniform working environment on any machine with ethernet/wi-fi, through a web browser. It is usable but still in progress. Also keep in mind that it's meant to use in direct connection, two reasons:

1. No SSL or any other security features.

2. Shell access is implemented in a very ad-hoc way. Client downloads whole shell history each couple seconds. With ethernet/wifi connection I don't care that much. :)

dependencies:
--------------

Any *nix based machine. It uses small utility I wrote in C. It's needed to have a shell feature.

https://github.com/BartekLew/box