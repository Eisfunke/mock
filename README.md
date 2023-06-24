# Mock

![wAR miR jEtzT Zu anSTRENgEnd dA JedeN ZWEiten BUChSTaBeN GRoSS zU scHREiBen](mock.png)

Mock is A GrEAt HAskeLL PrOGRaM to trANsForm tEXT. It comes as command line application and also offers an HTTP API for your remote mocking needs. I have a public instance running you can use. Take a look at the [API spec](https://software.pages.eisfunke.com/mock/index.html)!


## Installation

Mock is built using Nix, so you'll need to have a working Nix setup with flakes enabled. If you do, you can run mock by simply calling:

```shell
$ nix run git+https://git.eisfunke.com/software/mock -- --help
$ nix run git+https://git.eisfunke.com/software/mock#mock-web -- --help  # for the API
```

To build the binaries, just clone this repository or download a [release](https://git.eisfunke.com/software/mock/releases) and call:

```
$ nix build
```

This will link the build result, you can find the binaries in `result/bin`.


## Usage

`mock [STYLE] [TEXT]`

You can get help a list of possible mock styles with `mock`.

If no text or "-" is given, input is read from `stdin`.

### Examples

```
$ mock random This is a great program.
ThIs Is A greaT PROgrAM.

$ mock space This is a great program.
T h i s   i s   a   g r e a t   p r o g r a m.
```

Mock calls can of course be concatenated with pipes:

```
$ mock space This is a great program. | mock random
T h I S   i S   A   G R e A t   P R o g r a m .
```


## License

[![WTFPL-Badge](wtfpl.png)](http://www.wtfpl.net)
