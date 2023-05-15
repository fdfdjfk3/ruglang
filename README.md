# ruglang
A simple interpreted programming languages created specifically for beginners in programming.
The syntax is meant to be simple and readable, but explicit!

This is a major W.I.P. This program is useless right now.

proposed syntax:
```js
use "$std/random";
use "$std/io";

function main() {
  final var goal = randomint(1..100);
  var tries = 1;

  println("try %0, guess a number from 1 to 99", tries);
  var guess = readint();
  
  until guess == goal {
    tries += 1;
    println("try %0, guess a number from 1 to 99", tries);
    guess = readint();
  }

  println("you won! tries: %0", tries);
}
```
