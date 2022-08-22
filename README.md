# Pontifex
Minimalistic Pontifex/Solitaire Cypher GUI
![](https://raw.githubusercontent.com/delorum/pontifex/master/pontifex_v4.png)

Bruce Schneier's Solitaire Cypher implementation. That one: https://www.schneier.com/academic/solitaire/ It was called Pontifex in the book Cryptonomicon by Neil Stevenson, I like more that name.

Very simple, fully open-source. All actual cypher implementation is [Here](https://github.com/delorum/pontifex/blob/master/src/main/scala/pontifex/Pontifex.scala)

Fully covered with [Tests](https://github.com/delorum/pontifex/blob/master/src/test/scala/pontifex/PontifexTest.scala). Test Data was taken also from Bruce Schneier's [Site](https://www.schneier.com/wp-content/uploads/2015/12/sol-test.txt), so you can be sure, the implementation is correct.

# How to build and run
Java, Maven and Git are needed.
```
git clone git@github.com:delorum/pontifex.git
cd pontifex
mvn clean test assembly:single
java -jar target/pontifex-1-SNAPSHOT-jar-with-dependencies.jar
```

# Usage

## Encryption

- Select Keyword: any word of any length. Remember to use different keywords for each message! (for example, use constant prefix plus current date)
- You can press `Backspace` if you hit the wrong key. The deck will reshuffle back.
- You can see the deck below the keyword is shuffling with each letter you type.
- Press `Ctrl-E`
- Type your message
- You can see the deck is also shuffling with each typed letter, key sequence is generated under your message and encrypted message appear on the third line. So basically you press letters of open text and see the letters of cypher text. Like in Enigma :)
- You can use Arrow keys to navigate the cursor over your text to make corrections.
- Press `Ctrl-S` to copy encrypted message to clipboard.
- Press `Ctrl-Q` to close the program.

## Decryption

- Select Keyword which was used to encrypt message
- Press `Ctrl-E` twice: "Decoding mode" message will appear.
- You can cycle between Encoding and Decoding modes with `Ctrl-E`. Return to Keyword mode is impossible (only with restart).
- Type letters of cyphertext. Open text will appear above.
- Like in Encryption Mode you can use Arrow keys to navigate the cursor over your cyphertext to make corrections.
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

## Custom Config

You can provide custom config for the program:
```java -jar target/pontifex-1-SNAPSHOT-jar-with-dependencies.jar --config myconfig.conf```

Example of config file:
```
alphabet1  = АБГДЕЖЗИКЛМНОПРСТУФХЦЧШЫЮЯ1256789.,
alphabet2  = АВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЮЯ125679
cards      = АВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЮЯ125679АВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЫЮЯ125679АВ
cardColors = CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBRR
lang       = ru
replaces   = "Ъ6 Б6 Ь6 В8 ЁЕ ЙИ ЭЗ 3З ЩШ 0О 4Ч"
```

- `alphabet1` - alphabet for open text
- `alphabet2` - alphabet for cypher text

Alphabet1 and Alphabet2 lengthes must be equal

- `cards` - symbols to use for deck. Length must be (alphabet1 * 2) + 2
- `cardColors` - colors to use for cards. These are supported: R(ed), G(reen), B(lue), Y(ellow), C(yan).

- `lang` - language for the user interface. Only `ru` and `en` are supported right now.

- `replaces` - replace these additional symbols by those symbols from alphabet1 before encryption. For additional security.

Thats all. Have fun! I hope this program will be helpful.
