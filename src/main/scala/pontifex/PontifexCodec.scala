package pontifex

import com.googlecode.lanterna.SGR
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import pontifex.PontifexCodecMode.{Decoding, Encoding, Key}

import java.awt.Toolkit
import java.awt.datatransfer.{DataFlavor, StringSelection}
import java.io.{FileInputStream, InputStream, InputStreamReader}
import java.nio.charset.Charset
import java.util.Properties
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object PontifexCodec extends App {

  val version = scala.io.Source
    .fromInputStream(this.getClass.getResourceAsStream("/version.txt"))
    .getLines()
    .next

  private val (pontifex, lang) = if (args.contains("--config")) {
    args
      .dropWhile(_ != "--config")
      .slice(1, 2)
      .headOption
      .filter(_.nonEmpty) match {
      case Some(file) => loadFromConfigStream(new FileInputStream(file))
      case None => sys.error("provide path to config")
    }
  } else if (args.contains("ru")) {
    loadFromConfigStream(this.getClass.getResourceAsStream("/default2.conf"))
  } else {
    loadFromConfigStream(this.getClass.getResourceAsStream("/default.conf"))
  }

  def loadFromConfigStream(in: InputStream): (Pontifex, String) = {
    val config = new Properties()
    config.load(new InputStreamReader(in, Charset.forName("UTF-8")))
    val alphabet1 = config.getProperty("alphabet1")
    require(alphabet1 != null, "alphabet required")
    val alphabet2 = config.getProperty("alphabet2", alphabet1)
    val cards = {
      val cardSymbols =
        config.getProperty("cards", alphabet2 + alphabet2 + alphabet2.take(2))
      val cardColors = config.getProperty(
        "cardColors",
        alphabet2.map(_ => 'C') + alphabet2
          .map(_ => 'B') + alphabet2.take(2).map(_ => 'R')
      )
      cardSymbols.zip(cardColors).toArray
    }
    val lang = config.getProperty("lang", "en")
    require(lang == "ru" || lang == "en", "only ru and en lang are supported")

    val replaces = config.getProperty("replaces", "")
    val replacesMap: Map[Char, Char] =
      if (replaces.isEmpty) Map.empty
      else replaces.split(" ").map(kv => (kv.head, kv(1))).toMap
    (new Pontifex(alphabet1, alphabet2, cards, replacesMap), lang)
  }

  val keyWord = if (lang == "ru") "Ключ: " else "Key: "
  val deckWord = if (lang == "ru") "Карты: " else "Deck: "

  private var deck = pontifex.deck("")

  private val key = ArrayBuffer[Char]()
  private val keySequence = ArrayBuffer[Int]()
  private val openMessage = ArrayBuffer[Char]()
  private val encryptedMessage = ArrayBuffer[Char]()

  private val defaultTerminalFactory = new DefaultTerminalFactory()

  private var mode: PontifexCodecMode = PontifexCodecMode.Key
  private var charCount = 1
  private var keyIsHidden = false
  private var deckIsHidden = false
  private var messageIsHidden = false

  private val isDebug = args.contains("--debug")

  private val firstRowMap = Map(
    1 -> 1,
    2 -> 2,
    3 -> 3,
    4 -> 4,
    5 -> 5,
    6 -> -1,
    7 -> 6,
    8 -> 7,
    9 -> 8,
    10 -> 9,
    11 -> 10,
    12 -> -1,
    13 -> 11,
    14 -> 12,
    15 -> 13,
    16 -> 14,
    17 -> 15,
    18 -> -1,
    19 -> 16,
    20 -> 17,
    21 -> 18,
    22 -> 19,
    23 -> 20,
    24 -> -1,
    25 -> 21,
    26 -> 22,
    27 -> 23,
    28 -> 24,
    29 -> 25,
    30 -> -1,
    31 -> 26,
    32 -> 27,
    33 -> 28,
    34 -> 29,
    35 -> 30,
    36 -> -1,
    37 -> 31,
    38 -> 32,
    39 -> 33,
    40 -> 34,
    41 -> 35
  )

  private val term: Terminal = {
    val terminal = defaultTerminalFactory.createAWTTerminal()

    terminal.setSize(540 * 7, 500 * 4)
    val screenDimension = Toolkit.getDefaultToolkit.getScreenSize
    terminal.setLocation(
      (screenDimension.width - terminal.getWidth) / 2,
      (screenDimension.height - terminal.getHeight) / 2
    )
    terminal.setTitle("PONTIFEX")
    terminal.setVisible(true)
    terminal.setResizable(true)

    terminal.enableSGR(SGR.BOLD)
    terminal
  }
  term.flush()
  Thread.sleep(500)
  setColor('G')
  printCursorPositionAndCharCount()
  printKey()
  printDeck()
  setKeyModePosition()

  private def setKeyModePosition(): Unit = {
    if (lang == "ru") setAbsCurPos(7, 2) else setAbsCurPos(6, 2)
  }

  term.flush()

  while (true) {
    val k = term.readInput()
    if (isDebug) {
      println(
        s"${k.getKeyType} : ${k.getCharacter} : isCtrlDown=${k.isCtrlDown} : isAltDown=${k.isAltDown} : isShiftDown=${k.isShiftDown}"
      )
    }
    if (k.isCtrlDown) {
      k.getCharacter.toChar match {
        case 'q' =>
          term.close()
          sys.exit(0)
        case 'e' =>
          mode = switch(mode)
          setCharCount()
          printCursorPositionAndCharCount()
        case 'k' =>
          keyIsHidden = !keyIsHidden
          if (keyIsHidden) {
            hideKey()
          } else {
            showKey()
          }
          printCursorPositionAndCharCount()
        case 'd' =>
          deckIsHidden = !deckIsHidden
          if (deckIsHidden) {
            hideDeck()
          } else {
            printDeck()
          }
          printCursorPositionAndCharCount()
        case 'w' =>
          printCursorPositionAndCharCount(isMessageChanging = true)
          messageIsHidden = !messageIsHidden
          if (messageIsHidden) {
            if (mode == Encoding) hideOpenMessage() else hideEncryptedMessage()
          } else {
            if (mode == Encoding) showOpenMessage() else showEncryptedMessage()
          }
          printCursorPositionAndCharCount()
        case 'v' =>
          if (mode == Encoding) enterOpenMessage() else enterEncryptedMessage()
        case 's' =>
          val myString = encryptedMessage
            .grouped(35)
            .map(row => row.grouped(5).map(part => part.mkString).mkString(" "))
            .mkString("\n")
          val stringSelection = new StringSelection(myString)
          val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
          clipboard.setContents(stringSelection, null)
          printCursorPositionAndCharCount()
        case '1' =>
          deck = pontifex.step1(deck)
          printDeck()
        case '2' =>
          deck = pontifex.step2(deck)
          printDeck()
        case '3' =>
          deck = pontifex.step3(deck)
          printDeck()
        case '4' =>
          deck = pontifex.step4(deck)
          printDeck()
        case '5' =>
          deck = pontifex.step1Joker2(deck)
          printDeck()
        case '6' =>
          deck = pontifex.reverseStep4(deck)
          printDeck()
        case _ =>
      }
    } else {
      mode match {
        case PontifexCodecMode.Key =>
          keyMode(k)
        case PontifexCodecMode.Encoding =>
          encodingMode(k)
        case PontifexCodecMode.Decoding =>
          decodingMode(k)
        case _ =>
      }
    }
    term.flush()
  }

  private def switch(mode: PontifexCodecMode): PontifexCodecMode = {
    mode match {
      case Key =>
        setAbsCurPos(1, 5)
        Encoding
      case Encoding =>
        setRelCurPos(0, 2)
        Decoding
      case Decoding =>
        setRelCurPos(0, -2)
        Encoding
    }
  }

  private def keyMode(k: KeyStroke): Unit = {
    k.getKeyType match {
      case KeyType.Backspace =>
        if (key.nonEmpty) {
          setRelCurPos(-1, 0)
          term.putCharacter(' ')
          setRelCurPos(-1, 0)
          reverseShuffleDeck(key.remove(key.length - 1))
        }
      case KeyType.Character =>
        val c = k.getCharacter.toChar.toUpper
        if (pontifex.containsOpen(c)) {
          key += c
          setColor('G')
          if (!keyIsHidden) term.putCharacter(c) else term.putCharacter(' ')
          shuffleDeck(c)
        }
      case _ =>
    }
    printCursorPositionAndCharCount()
  }

  private def decodingMode(k: KeyStroke): Unit = {
    k.getKeyType match {
      case KeyType.ArrowUp =>
        setRelCurPos(0, -1)
        setCharCount()
      case KeyType.ArrowDown =>
        setRelCurPos(0, 1)
        setCharCount()
      case KeyType.ArrowLeft =>
        setRelCurPos(-1, 0)
        setCharCount()
      case KeyType.ArrowRight =>
        setRelCurPos(1, 0)
        setCharCount()
      case KeyType.Character =>
        val c = k.getCharacter.toChar.toUpper
        decode(c)
      case _ =>
    }
    printCursorPositionAndCharCount()
  }

  private def decode(c: Char): Unit = {
    if (pontifex.containsEncrypted(c) && charCount != -1 && (charCount - 1) <= encryptedMessage.length) {
      putEncryptedChar(c)
      if (keySequence.length < charCount) {
        val number: Int = generateNextKeyNumber
        keySequence += number
      }
      setRelCurPos(-1, -1)
      putKeyNumber()
      setRelCurPos(-1, -1)
      val decryptedC = pontifex.decryptSymbol(c, keySequence(charCount - 1))
      putOpenChar(decryptedC)
      setRelCurPos(0, 2)
      moveCaret()
    }
  }

  private def encodingMode(k: KeyStroke): Unit = {
    k.getKeyType match {
      case KeyType.ArrowUp =>
        setRelCurPos(0, -1)
        setCharCount()
      case KeyType.ArrowDown =>
        setRelCurPos(0, 1)
        setCharCount()
      case KeyType.ArrowLeft =>
        setRelCurPos(-1, 0)
        setCharCount()
      case KeyType.ArrowRight =>
        setRelCurPos(1, 0)
        setCharCount()
      case KeyType.Enter =>
        encode(pontifex.getRandomOpenSymbol)
      case KeyType.Character =>
        val c = k.getCharacter.toChar.toUpper
        encode(c)
      case _ =>
    }
    printCursorPositionAndCharCount()
  }

  private def encode(c: Char): Unit = {
    if (pontifex.containsOpen(c) && charCount != -1 && (charCount - 1) <= openMessage.length) {
      putOpenChar(c)
      if (keySequence.length < charCount) {
        val number: Int = generateNextKeyNumber
        keySequence += number
      }
      setRelCurPos(-1, 1)
      putKeyNumber()
      setRelCurPos(-1, 1)
      val encryptedC = pontifex.encryptSymbol(c, keySequence(charCount - 1))
      putEncryptedChar(encryptedC)
      setRelCurPos(0, -2)
      moveCaret()
    }
  }

  private def putKeyNumber(): Unit = {
    if (!messageIsHidden) {
      val (card, color) = pontifex.getCard(keySequence(charCount - 1))
      setColor(color)
      term.putCharacter(card)
    } else term.putCharacter(' ')
  }

  private def putOpenChar(c: Char): Unit = {
    if (!messageIsHidden || mode == Decoding) {
      setColor('G')
      term.putCharacter(c)
    } else {
      term.putCharacter(' ')
    }
    if (charCount > openMessage.length) openMessage += c
    else openMessage(charCount - 1) = c
  }

  private def putEncryptedChar(c: Char): Unit = {
    if (!messageIsHidden || mode == Encoding) {
      setColor('Y')
      term.putCharacter(c)
    } else {
      term.putCharacter(' ')
    }
    if (charCount > encryptedMessage.length) encryptedMessage += c
    else encryptedMessage(charCount - 1) = c
  }

  private def reverseShuffleDeck(c: Char): Unit = {
    pontifex.getNumberForOpenSymbol(c).foreach { amount =>
      deck = pontifex.reverseCountCut(amount, deck)
      term.flush()
      if (!deckIsHidden) Thread.sleep(100)
      deck = pontifex.reverseStep4(deck)
      printDeck()
      term.flush()
      if (!deckIsHidden) Thread.sleep(100)
      deck = pontifex.step3(deck)
      printDeck()
      term.flush()
      if (!deckIsHidden) Thread.sleep(100)
      deck = pontifex.reverseStep2(deck)
      printDeck()
      term.flush()
      deck = pontifex.reverseStep1(deck)
      printDeck()
    }
  }

  private def shuffleDeck(c: Char): Unit = {
    pontifex.getNumberForOpenSymbol(c).foreach { amount =>
      deck = pontifex.step1(deck)
      printDeck()
      term.flush()
      if (!deckIsHidden) Thread.sleep(100)
      deck = pontifex.step2(deck)
      printDeck()
      term.flush()
      if (!deckIsHidden) Thread.sleep(100)
      deck = pontifex.step3(deck)
      printDeck()
      term.flush()
      if (!deckIsHidden) Thread.sleep(100)
      deck = pontifex.step4(deck)
      printDeck()
      term.flush()
      deck = pontifex.countCut(amount, deck)
    }
  }

  @tailrec
  private def generateNextKeyNumber: Int = {
    deck = pontifex.step1(deck)
    printDeck()
    term.flush()
    if (!deckIsHidden) Thread.sleep(100)
    deck = pontifex.step2(deck)
    printDeck()
    term.flush()
    if (!deckIsHidden) Thread.sleep(100)
    deck = pontifex.step3(deck)
    printDeck()
    term.flush()
    if (!deckIsHidden) Thread.sleep(100)
    deck = pontifex.step4(deck)
    printDeck()
    term.flush()
    val (number, _) = pontifex.step5(deck)
    number match {
      case pontifex.Joker1 | pontifex.Joker2 => generateNextKeyNumber
      case x => x
    }
  }

  private def setAbsCurPos(x: Int, y: Int): Unit = {
    term.setCursorPosition(x, y)
  }

  private def setRelCurPos(x: Int, y: Int): Unit = {
    term.setCursorPosition(curPos.getColumn + x, curPos.getRow + y)
  }

  private def curPos = {
    term.getCursorPosition
  }

  private def toStr(int: Int): String = {
    if (int >= 0 && int < 10) s"0$int" else int.toString
  }

  private def printCursorPositionAndCharCount(
      isMessageChanging: Boolean = false): Unit = {
    val pos = curPos
    def saveHotkey = if (lang == "ru") "Ctrl-S:Сохр" else "Ctrl-S:Sav"
    def pasteHotkey = if (lang == "ru") "Ctrl-V:Вст" else "Ctrl-V:Ins"
    def exitHotkey = if (lang == "ru") "Ctrl-Q:Вых" else "Ctrl-Q:Quit"
    def keyHotkey =
      if (lang == "ru") {
        if (keyIsHidden) "Ctrl-K:ОткКл" else "Ctrl-K:СкрКл"
      } else {
        if (keyIsHidden) "Ctrl-K:ShwKey" else "Ctrl-K:HidKey"
      }
    def deckHotkey =
      if (lang == "ru") {
        if (deckIsHidden) "Ctrl-D:ОткКар" else "Ctrl-D:СкрКар"
      } else {
        if (deckIsHidden) "Ctrl-D:ShwDec" else "Ctrl-D:HidDec"
      }
    def messageHotkey =
      redify(if (lang == "ru") {
        if (messageIsHidden) "Ctrl-W:ОткТек" else "Ctrl-W:СкрТек"
      } else {
        if (messageIsHidden) "Ctrl-W:ShwTex" else "Ctrl-W:HidTex"
      })
    def redify(str: String): String = if (isMessageChanging) s"[R$str]" else str
    def enterHotkey = if (lang == "ru") "Enter:Слч" else "Enter:Rnd"
    val hotkeys =
      s"${mode.hotkey(lang)} | $saveHotkey | $pasteHotkey | $exitHotkey | $keyHotkey | $deckHotkey | $messageHotkey | $enterHotkey"
    val str =
      s"v$version | ${toStr(pos.getColumn)} : ${toStr(pos.getRow)} ($charCount) | ${mode
          .name(lang)} | $hotkeys     "
    setAbsCurPos(1, 1)
    setColor('G')
    printString(str)
    setAbsCurPos(pos.getColumn, pos.getRow)
    if (isDebug) {
      println(openMessage.mkString)
      println(encryptedMessage.mkString)
    }
    term.flush()
  }

  private def printKey(): Unit = {
    val pos = curPos
    setAbsCurPos(1, 2)
    setColor('G')
    printString(keyWord)
    setAbsCurPos(pos.getColumn, pos.getRow)
  }

  private def hideKey(): Unit = {
    val pos = curPos
    setAbsCurPos(1, 2)
    setColor('G')
    printString(keyWord)
    printString(" " * key.length)
    setAbsCurPos(pos.getColumn, pos.getRow)
  }

  private def hideOpenMessage(): Unit = {
    val pos = curPos
    setAbsCurPos(1, 5)
    setCharCount()
    keySequence.foreach { _ =>
      term.putCharacter(' ')
      setRelCurPos(-1, 1)
      term.putCharacter(' ')
      setRelCurPos(0, -1)
      moveCaret()
      Thread.sleep(100)
      term.flush()
    }
    setAbsCurPos(pos.getColumn, pos.getRow)
    term.flush()
  }

  private def hideEncryptedMessage(): Unit = {
    val pos = curPos
    setAbsCurPos(1, 7)
    setCharCount()
    keySequence.foreach { _ =>
      term.putCharacter(' ')
      setRelCurPos(-1, -1)
      term.putCharacter(' ')
      setRelCurPos(0, 1)
      moveCaret()
      Thread.sleep(100)
      term.flush()
    }
    setAbsCurPos(pos.getColumn, pos.getRow)
    term.flush()
  }

  private def showOpenMessage(): Unit = {
    val pos = curPos
    setAbsCurPos(1, 5)
    setCharCount()
    keySequence.foreach { c =>
      setColor('G')
      term.putCharacter(openMessage(charCount - 1))
      setRelCurPos(-1, 1)
      val (card, color) = pontifex.getCard(c)
      setColor(color)
      term.putCharacter(card)
      setRelCurPos(0, -1)
      moveCaret()
      Thread.sleep(100)
      term.flush()
    }
    setAbsCurPos(pos.getColumn, pos.getRow)
    term.flush()
  }

  private def showEncryptedMessage(): Unit = {
    val pos = curPos
    setAbsCurPos(1, 7)
    setCharCount()
    keySequence.foreach { c =>
      setColor('Y')
      term.putCharacter(encryptedMessage(charCount - 1))
      setRelCurPos(-1, -1)
      val (card, color) = pontifex.getCard(c)
      setColor(color)
      term.putCharacter(card)
      setRelCurPos(0, 1)
      moveCaret()
      Thread.sleep(100)
      term.flush()
    }
    setAbsCurPos(pos.getColumn, pos.getRow)
    term.flush()
  }

  def enterOpenMessage(): Unit = {
    val text = Toolkit.getDefaultToolkit.getSystemClipboard.getData(DataFlavor.stringFlavor).asInstanceOf[String]
    text.foreach { c =>
      encode(c.toUpper)
      Thread.sleep(100)
      term.flush()
    }
  }

  def enterEncryptedMessage(): Unit = {
    val text = Toolkit.getDefaultToolkit.getSystemClipboard.getData(DataFlavor.stringFlavor).asInstanceOf[String]
    text.foreach { c =>
      decode(c.toUpper)
      Thread.sleep(100)
      term.flush()
    }
  }

  private def showKey(): Unit = {
    val pos = curPos
    setAbsCurPos(1, 2)
    setColor('G')
    printString(keyWord)
    printChars(key)
    setAbsCurPos(pos.getColumn, pos.getRow)
  }

  private def hideDeck(): Unit = {
    val pos = curPos
    setAbsCurPos(1, 3)
    setColor('G')
    printString(deckWord)
    printString(" " * deck.length)
    setAbsCurPos(pos.getColumn, pos.getRow)
  }

  private def printDeck(): Unit = {
    if (!deckIsHidden) {
      val pos = curPos
      setAbsCurPos(1, 3)
      setColor('G')
      printString(deckWord)
      deck.foreach(printCard)
      setAbsCurPos(pos.getColumn, pos.getRow)
    }
  }

  private def printCard(c: Int): Unit = {
    val (card, color) = pontifex.getCard(c)
    setColor(color)
    term.putCharacter(card)
  }

  private var currentColor: Char = 'G'

  private def setColor(color: Char): Unit = {
    color match {
      case 'R' => term.setForegroundColor(ANSI.RED)
      case 'G' => term.setForegroundColor(ANSI.GREEN)
      case 'B' => term.setForegroundColor(ANSI.BLUE)
      case 'Y' => term.setForegroundColor(ANSI.YELLOW)
      case 'C' => term.setForegroundColor(ANSI.CYAN)
      case _ => sys.error(s"unexpected color $color")
    }
    currentColor = color
  }

  private def printString(str: String): Unit = {
    printChars(str.toCharArray)
  }

  private def printChars(str: Seq[Char]): Unit = {
    val colorStack = ArrayBuffer[Char]()
    val ColorNoMode = 0
    val ColorSet = 1
    var colorMode = 0
    str.foreach {
      case '[' =>
        colorMode = ColorSet
      case c @ ('R' | 'G' | 'B' | 'Y' | 'C') =>
        if (colorMode == ColorSet) {
          colorStack += currentColor
          setColor(c)
          colorMode = ColorNoMode
        } else {
          term.putCharacter(c)
        }
      case ']' =>
        if (colorStack.nonEmpty) {
          val prevColor = colorStack.remove(colorStack.length - 1)
          setColor(prevColor)
        }
      case c =>
        term.putCharacter(c)
    }
  }

  private def moveCaret(): Unit = {
    charCount += 1
    if ((charCount - 1) % 350 == 0) {
      val yPos = if (mode == Encoding) 5 else 7
      if (curPos.getColumn > 44) setAbsCurPos(89, yPos)
      else setAbsCurPos(45, yPos)
    } else if ((charCount - 1) % 35 == 0) {
      if (curPos.getColumn > 88) setAbsCurPos(89, curPos.getRow + 4)
      else if (curPos.getColumn > 44) setAbsCurPos(45, curPos.getRow + 4)
      else setAbsCurPos(1, curPos.getRow + 4)
    } else if ((charCount - 1) % 5 == 0) {
      setRelCurPos(1, 0)
    }
  }

  @tailrec
  private def getCharCount(x: Int, y: Int): Int = {
    if (x > 44) getCharCount(x - 44, y + 45 - 5)
    else {
      if (mode == PontifexCodecMode.Encoding) {
        if ((y - 1) % 4 != 0) -1
        else {
          val add = ((y - 1) / 4 - 1) * 35
          val firstPart = firstRowMap.getOrElse(x, -1)
          if (firstPart == -1) -1
          else firstPart + add
        }
      } else {
        if ((y - 3) % 4 != 0) -1
        else {
          val add = ((y - 3) / 4 - 1) * 35
          val firstPart = firstRowMap.getOrElse(x, -1)
          if (firstPart == -1) -1
          else firstPart + add
        }
      }
    }
  }

  private def setCharCount(): Unit = {
    charCount = getCharCount(curPos.getColumn, curPos.getRow)
  }
}
