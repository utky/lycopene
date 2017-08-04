# Design doc

Simple&Smallを原則にする。

## View component tree

* main
  * nav (left)
    * current sprint name
    * menu
      * project
    * user & preference
  * content (right)
    * sprint (top)
      * dead line
      * pomodoro state
    * issues  (body)
      * list (left)
      * detail (right & hidden default)
        * title
        * description
        * record summary
        * pomodoro action

## keyboard friendly

keyboard shortcutとの親和性を最大化するためのUI構造が何かを考える。
基点となるデータを決める。Vimであれば行が基点だろう。
hjklで移動
iでinsert -> new
ddでdelete
みたいな

hmmm 行列だ。
project, sprint, issue のcolumnを持つ
あれ、itunesのUIじゃないか！あるいはFinder
とりあえず3columnだけを実装してみるか


## notes

機能とモジュールの関連をどうするか。
使う立場で考えると、モジュールが提供するインタフェースが
抽象の組み立てに貢献するほうがいい。

CmdやHtmlが合成は保証してくれる。

DomainとView, Message, Updateの分解をどうするか。
やはりDomainはデータと制約・規則を表すpureな世界であってほしい。

どうだろここでストリーム処理っぽいデータフローの概念を活かせないかな。
Widgetの関数とDoaminの関数をpipelineするイメージ
イベントはWidgetで終端したりドメインで終端したり。
ユーザはWidget(metaphor)と対話する。

```
      Event          Event
User ------> Widget -------> Domain
```

いや、ユーザはDomainと対話するべきなのか？
最初からDomainと呼んでいるものがユーザのメンタルモデルと一致している必要が？

なんというかアプリケーションのドメインとユーザのドメインは必ずしも
一致しないという気がしている。それでいいのか謎だけど。
ビジネスのドメインで扱うのは蒸留された何かだ。

ユーザが**何と対話するのか**を基点に考えた方がいいな。
ユーザは非正規化されたモデルを扱うけれど、アプリケーション内部では
正規化することでプログラムを単純化できる。

先にユーザサイドからエンコードした方がいいか、
ドメインモデルからエンコードした方がいいか。
あくまでもUIなのだしユーザが見るモデルを直接エンコードした方がいい気がする。

*たとえば仕事では完全にViewそのものを構造化して引き回すような感じでreactやってた*
ただ、抽象度が低いと合成が難しくてadhocな部品ばかりになる。
そうか、結局widgetという部品とspecificなviewモデルの関係をどうするかを
考えたいだけだな。

で、specificなモデルをどういうパッケージ構成にするかの問題だ。
`Project`などのpure domainっぽいものはそのままトップレベルでいいか。
いまここに書いているのはwidget層にもgenericとspecificなモデルがあるという話か。
ViewとWidgetでモジュールを分けるのがよさそう。

ただし、Msgとどこで定義してどこでハンドルさせるかが決まっていない。
Msgはモジュールと対話するためのルールだ。

widgetってレンダリングだけ気にすればいい、ってことはないか。
interactionもセットなのでやはりmsg関連の関数を埋め込んだりすることになる。

2016-11-25
絵を書いてみて思うけれど、やはりドメインモデルとUIはシームレスじゃない。

- ドメイン
  - モデルの構造についての設計
- ビュー
  - widgetとviewの分離
  - widgetの汎用性

```
Widget

view : 

### データのもちかた

Web APIにもよるけれどnest by nestしてる構造は
横断的なデータフィルタに苦労するという気がする。
それを構造から解決しようとすると**結局JSのnornmalizrの再導入になる**。
データコレクションを連想配列で保持するような形。
かなり木構造からはずれる代わりにデータの扱いは楽になる。

木構造を崩すとデータの関係が不透明になるのでドメインの仕様・制約を
表現する力がかなり薄まってしまって気持ち悪い。せっかく設計したドメインを壊すようで。
CQRSみたいにCommandのターゲットはドメイン(正規化)で、Queryが
連想配列ならまあ解る。

けれどViewの層でそれらを二つ持ちするのは馬鹿らしい。

## Core concept

- Tracking issues and records
- Currently I'm using Wunderlist
  - Will app replace it? No.
- Visualising and analysing log.
- Less mouse action. (good support for keyboard shortcut)

## User's action

1. Open top page
2. Overview tasks
3. Select project
4. View backlog and current issues
5. Select sprint and view targeted issues.
  - We must work only one sprint. (No parallel sprints)

左のNavからProject -> Sprintまで辿れるのがいいな。
メインにはSprintの概況とissuesの一覧、最近の活動が見えればいい

## Point of view as pomodoro technique 

1. What to do.
2. Choose tasks to be got done today.
3. Sort tasks by priority.
4. Enter pomodoro time.
5. Maybe inturruption. -> Mark it as interrupted.
6. Take a break.
