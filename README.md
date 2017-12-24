# hsocks

hsocks allows you to redirect any TCP connection to a SOCKS or HTTP proxy.

## building

```
# stack build
# stack install
```

## usage

```
hsocks -h 127.0.0.1 -l 3333 -t http -x 127.0.0.1 -p 8080
sudo iptables -t nat -A OUTPUT -d 127/8 -j RETURN
sudo iptables -t nat -A OUTPUT -d 192.168/16 -j RETURN
sudo iptables -t nat -A OUTPUT -d 10/8 -j RETURN
sudo iptables -t nat -A OUTPUT -d 172.16/12 -j RETURN
sudo iptables -t nat -A OUTPUT -p tcp -j REDIRECT --to-ports 3333
```
