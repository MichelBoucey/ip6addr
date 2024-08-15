# ip6addr ![CI](https://github.com/MichelBoucey/ip6addr/actions/workflows/haskell-ci.yml/badge.svg)

## 1. Goal

ip6addr is a commandline tool that validates and generates IPv6 address text representations:

* Canonical, in conformation with RFC 5952 (default output)
* Pure, i.e. rewriting IPv4 address if present
* Full length
* Reverse lookup domain name, in conformation with RFC 3596 Section 2.5
* Windows UNC path name
* Random generation for test purpose  

## 2. Usage

```
ip6addr v2.0.0 (c) Michel Boucey 2011-2024

Usage: ip6addr [(-c|--canonical) | (-n|--no-ipv4) | (-f|--full-length) |
                 (-p|--ptr) | (-w|--windows-unc) | (-r|--random)]
               [-q|--quantity ARG] [-x|--prefix ARG] [<IPv6 address>]

  ip6addr

Available options:
  -c,--canonical           In conformation with RFC 5952 (default)
  -n,--no-ipv4             Force the removal of IPv4 address if necessary
  -f,--full-length         Full IPv6 address length
  -p,--ptr                 PTR reverse mapping
  -w,--windows-unc         Windows UNC path name
  -r,--random              Random generation
  -q,--quantity ARG        Amount of random addresses to generate
  -x,--prefix ARG          Set a prefix for random addresses to generate
  -h,--help                Show this help text
```

## 3. Examples

```bash
    [user@box ~]$ ip6addr 0:0::FFFF:192.0.2.128  
    ::ffff:192.0.2.128  
    [user@box ~]$ ip6addr -n 0:0::FFFF:192.0.2.128  
    ::ffff:c000:280  
    [user@box ~]$ ip6addr -p 0:0::FFFF:192.0.2.128  
    0.8.2.0.0.0.0.c.f.f.f.f.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.IP6.ARPA.  
    [user@box ~]$ ip6addr -w 2001:db8:85a3:8d3:1319:8a2e:370:7348
    2001-db8-85a3-8d3-1319-8a2e-370-7348.ipv6-literal.net    
    [user@box ~]$ ip6addr -f 0:0::FFFF:192.0.2.128  
    0000:0000:0000:0000:0000:ffff:c000:0280  
    [user@box ~]$ ip6addr -r  
    ca18::654a:57ab:2ed7  
    [user@box ~]$ ip6addr -r -q 4  
    488:f7f3:7f7e::6ffd  
    5b8b:8ab4::f0b8  
    f191:ecc9:8193:8a2:104c:fec1:0:8c4d  
    af3b:3ffd:8302:6c56::8623:26  
    [user@box ~]$ ip6addr -r -q 3 -p 1234:1ab2::  
    1234:1a2b::cda6:cf2a:bb1d:d70b  
    1234:1a2b::362c:8295:f546:a43a  
    1234:1a2b::1121:ca16:ab5:688a  
```

