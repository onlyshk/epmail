## What
EPmail - is mail system written in Erlang.

## Install
    make

## Runing
In erlang shell:

    popd_sup:start_link().

Add user 3 parameters: Domain Name Password

    maildir:adduser("localhost/", "admin", "123").

## Test
You can connect to servers by telnet or you can use mail client, thunderbird for example:

    [shk@myhost ~]$ telnet localhost 110
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    +OK POP3 server ready 
    use shk
    -ERR
    user admin
    +OK
    pass 123
    +OK
    list 1
    +OK 1 3507
    .
    noop
    +OK

## Contribute
 1) Fork epmail
 
 2) Write some new features or fix bug
 
 3) Test it
 
 4) Pull request

## Wiki
  EPmail wiki - [EPmail wiki]:[[https://github.com/onlyshk/epmail/wiki]]
 
## EPmail TODO and Issues
  EPmail issues - [EPmail issues]:(https://github.com/onlyshk/epmail/issues "issues")

## More info
  More info - kuleshovmail@gmail.com
