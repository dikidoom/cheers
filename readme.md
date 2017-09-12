# Cheers
When more than 3 people are having drinks and want to toast each other individually, things get complicated. Luckily, this program will help you find 
- the *optimal* way of toasting each other
- with minimal fuss (least moves) and
- without crossing drinks!

## Usage
```
$ racket
> (enter! "main.rkt")
> (define foo (solve n))  ; where n is number of people, preferably < 9
> (vis foo)               ; saves an instructional PNG
```

## Example
![Example](http://pdikmann.github.io/cheers.png)
How could you ever have done without it?!
