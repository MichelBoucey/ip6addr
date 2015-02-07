## ip6addr is a commandline tool written in Haskell that generates IPv6 address text representations

* Canonical, in conformation with RFC 5952 (default output)
* Pure, i.e. rewriting IPv4 address if present
* Full length
* Reverse lookup domain name, in conformation with RFC 3596 Section 2.5
* Random generation for test purpose  

&gt; ip6addr -a 0:0::FFFF:192.0.2.128  
::ffff:192.0.2.128  
&gt; ip6addr -o pure -a 0:0::FFFF:192.0.2.128  
::ffff:c000:280  
&gt; ip6addr -o arpa -a 0:0::FFFF:192.0.2.128  
0.8.2.0.0.0.0.c.f.f.f.f.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.IP6.ARPA.  
&gt; ip6addr -o full -a 0:0::FFFF:192.0.2.128  
0000:0000:0000:0000:0000:ffff:c000:0280  
&gt; ip6addr -o random  
ca18::654a:57ab:2ed7  
&gt; ip6addr -o random -q 4  
488:f7f3:7f7e::6ffd  
5b8b:8ab4::f0b8  
f191:ecc9:8193:8a2:104c:fec1:0:8c4d  
af3b:3ffd:8302:6c56::8623:26
&gt; ip6addr -o random -q 3 -p 1234:1ab2::
1234:1a2b::cda6:cf2a:bb1d:d70b
1234:1a2b::362c:8295:f546:a43a
1234:1a2b::1121:ca16:ab5:688a
