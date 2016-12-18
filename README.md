# jgrep

`jgrep` provides the ability to sift through JSON-structured log files, such as those produced by [Logrus](https://github.com/sirupsen/logrus), using a simple and intuitive query syntax.

Examples, documentation, and notes on architecture to come. The source for the whole program is quite small - less than 300LOC as of right now - so please do read around. It's poorly documented but hopefully the types speak for themselves. The intuition is that jgrep is a compiler which turns an English-like query syntax into a small [jq](https://github.com/stedolan/jq) script, then simply pipes JSON from stdin through that jq script to obtain a resulting JSON object, which it prints to stdout.
