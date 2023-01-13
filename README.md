# Pontifex
Minimalistic Pontifex/Solitaire Cipher GUI
![](https://raw.githubusercontent.com/delorum/pontifex/master/pontifex_v4.png)

Bruce Schneier's Solitaire Cipher implementation. That one: https://www.schneier.com/academic/solitaire/ It was called Pontifex in the book Cryptonomicon by Neil Stevenson, I like more that name.

Very simple, fully open-source. All actual cipher implementation is [Here](https://github.com/delorum/pontifex/blob/master/src/main/scala/pontifex/Pontifex.scala)

Fully covered with [Tests](https://github.com/delorum/pontifex/blob/master/src/test/scala/pontifex/PontifexTest.scala). Test Data was taken also from Bruce Schneier's [Site](https://www.schneier.com/wp-content/uploads/2015/12/sol-test.txt), so you can be sure, the implementation is correct.

![](https://github.com/delorum/pontifex/actions/workflows/maven_test.yml/badge.svg?branch=master)

# About Solitaire

In short, this is a [stream cipher](https://en.wikipedia.org/wiki/Stream_cipher) with pseudorandom keystream, generated by a deck of playing cards. Main part of the algorithm is a keystream generation process.  Keystream must be random or look random, that is important. It's like a [one-time pad](https://en.wikipedia.org/wiki/One-time_pad), but more convenient - you don't need to keep somewhere notes with numbers, you can generate them whenever you need. Yes, they are not actually random, but look so. No (short) loops. And they are inifinite - you can encrypt messages of any length.

<details>
<summary><b>More info</b></summary>

Encryption is simple: just add numbers for your letters (A - 1, B - 2, etc) to numbers from keystream. Subtract 26 if the number is bigger and convert back to letter. Decryption is the same process backwards. If the keystream is random your encrypted text would also look like random bunch of symbols. Like you use [Vigenère cipher](https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher) with randomly chosen alphabet for each letter without repetitions (e.g. you have inifinite key).

Generation is done by deck of cards. It was meant to operate without calculation machines - only cards, pen and paper. But this process is not easy and not fast. One mistake and all encryption is screwed. This program takes this part from you and perform all operations by itself. Yet it draws the deck (minimalistically) during the process and actually shuffles it! (for your entertainment and feel of control. Can be disabled)

So why to use it instead of industry-proven ciphers like AES and others? Well, for fun, of course, in first place. For some not very improtant private things like your paper diary. Great benefit is that you deal with letters, not numbers. The length of cipertext is the same as opentext and is encoded to the same letters. That is convenient when you need to write it on paper. Encryption is done letter-by-letter - also convenient. You don't need to write somewhere an open text and then pass it to encryption program/ You can encrypt it in place without any trace.

The cipher is very easy to understand. All operations are simple - just shuffling. Yet it is secure. You can find some info about cryptoanalysis on Schneier's site. Last paper is from [2019](https://www.schneier.com/blog/archives/2019/10/more_cryptanaly.html). The cipher itself was writen in 1999 - 23 years ago (in 2022) and still there are no evidence of any vulnerabilities (of course maybe they didn't search thoroughly or don't tell).

Just remember not to use the same keystream twice. One can subtract one such cipher text from another - that is the same as subtraction of one open text from another which is a straight way to breaking both messages! Simple solution is to add a current date to your constant keyword.

</details>

# How to build and run
Java, Maven and Git are needed.
```
git clone git@github.com:delorum/pontifex.git
cd pontifex
mvn clean test assembly:single
mv target/*.jar pontifex.jar
java -jar pontifex.jar
```

# Usage

## Encryption

- Select Keyword: any word of any length. Remember to use different keywords for each message! (for example, use constant prefix plus current date) or it would be very easy to decrypt your messages.
- You can press `Backspace` if you hit the wrong key. The deck will reshuffle back.
- You will see the deck below the keyword shuffling with each letter you type.
- When you finish to enter your keyword the deck will be very well shuffled. This is the initial state of your keystream generator.
- Press `Ctrl-E`. This will enter the program to Encoding Mode
- Type your message you want to encrypt.
- You can see the deck is also shuffling with each typed letter, key sequence is generated under your message and encrypted message appear on the third line. So basically you type letters of open text and see the letters of cipher text. Like in Enigma. The encryption process is very informative.
- You can use `Arrow keys` to navigate the cursor over your text to make corrections (beat Enigma in that case).
- Press `Ctrl-S` to copy encrypted message to clipboard.
- Press `Ctrl-Q` to close the program.

## Decryption

- Select Keyword which was used to encrypt message
- Press `Ctrl-E` twice: "Decoding mode" message will appear.
- You can cycle between Encoding and Decoding modes with `Ctrl-E`. Return to Keyword mode is impossible (only with restart).
- Type letters of ciphertext. Open text will appear above.
- Like in Encryption Mode you can use Arrow keys to navigate the cursor over your ciphertext to make corrections.
- Press `Ctrl-Q` to close the program. No data will be saved to any files

## Other Hotkeys

- `Ctrl-K`: show/hide the keyword
- `Ctrl-D`: show/hide the deck. Encryption and decryption would perform faster with deck being hidden.
- `Ctrl-W`: show/hide open message and key sequence.
- With these three hotkeys it is possible to enter fully minimalistic mode: only ciphertext or opentext would appear on typing.
- `Ctrl-1,2,3,4` - perform operation on the deck: step1, step2, step3, step4 of the Solitaire/Pontifex algorithm
- `Ctrl-5` - move Joker2 by 1 card (like step1 for Joker1)
- With `Ctrl-1` and `Ctrl-5` you can place Jokers in any positions you want before starting to enter keyword (for additional security).
- `Ctrl-6` - reverse for step4. This is much for debug purposes
- `Enter` - generates random letter and encrypts it. You can add them at the end of your message to hide its actual length.

## Custom Config

You can provide custom config for the program:
```
java -jar target/pontifex.jar --config myconfig.conf
```

Example of config file (this is config1.conf in the repo - one Russian variant):
```
alphabet1  = АБГДЕЖЗИКЛМНОПРСТУФХЦЧШЫЮЯ1256789.,
alphabet2  = АВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЮЯ125679
cards      = АВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЮЯ125679АВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЮЯ125679АВ
cardColors = CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBRR
lang       = ru
replaces   = "Ъ6 Б6 Ь6 В8 ЁЕ ЙИ ЭЗ 3З ЩШ 0О 4Ч"
```

- `alphabet1` - (required) alphabet for open text. This is an extension to the standard cipher. You can use any symbols and any alphabet length you need, not just latin alphabet.
- `alphabet2` - (optional) alphabet for cipher text. You can use any other symbols to hide your original symbols set. Default: alphabet1.

Alphabet1 and Alphabet2 lengthes must be equal

- `cards` - (optional) symbols to use for deck. Length must be (alphabet1 * 2) + 2. Default: alphabet2 twice plus first two symbols for jokers
- `cardColors` - (optional) colors to use for cards. These are supported: R(ed), G(reen), B(lue), Y(ellow), C(yan). Default: half of the deck is cyan, half is blue, red for two jokers.

- `lang` - (optional) language for the user interface. Only `ru` and `en` are supported right now. Default is en.

- `replaces` - (optional) replace these additional symbols by those symbols from alphabet1 before encryption. For additional security.

Thats all. Have fun! I hope this program will be helpful.

And EUELY WCGNS VNBKP BWZTJ
