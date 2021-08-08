This branch is required for signing the Guix channel.

An example of adding developer's key to the channel:

```
gpg --list-public-keys --keyid-format long | grep S
gpg --armor --export 029D8EB77E18D68C > frewacom-029D8EB77E18D68C.key
```
