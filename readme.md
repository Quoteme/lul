# Lul - Window manager

## Development requirements

- xorg-server-xephyr

```
Xephyr :1 -ac -br -screen 1024x768 -resizeable -reset -terminate
export DISPLAY=:1
```

```
cabal run
export DISPLAY=:0
```
