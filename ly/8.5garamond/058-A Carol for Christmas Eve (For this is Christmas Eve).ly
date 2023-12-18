\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"A Carol for Christmas Eve"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"Traditional"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #058
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key g \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(0 . 0)
}

sopMusic = \relative c'' {
  \partial 4 b8[ c] |
  d4 c b a8[ g] |
  c4 b a g |
  fis e b' b |
  
  e,2. \bar""\break b'8[ c] |
  d4 c b a8[ g] |
  c4 b a g |
  fis e b' b |
  \partial 2. e,2. \bar "||" \break
  
  \partial 4 fis4 |
  g4. fis8 g4 a8[ b] |
  a4 g fis d |
  g g d' a |
  b2. \bar""\break e4 |
  
  d e b a8[ g] |
  c4 b a g |
  fis e b' b |
  \partial 2. e,2. \bar "||" \break
  
  \partial 4 fis4 |
  g4. fis8 g4 a8[ b] |
  a4 g fis d |
  g g d' a |
  b2. \bar""\break e4 |
  
  d4 e b a8[ g] |
  c4 b a g |
  fis e b' b |
  \partial 2. e,2. \bar ":|." \break
  
  
  
  \partial 4 b'8[ c] |
  d4 c b a8[ g] |
  c4 b a g |
  fis e b' b |
  
  e,2. \bar""\break b'8[ c] |
  d4 c b a8[ g] |
  c4 b a g |
  fis e b' b |
  \partial 2. e,2. \bar "||" \break
  
  \partial 4 fis4 |
  g4. fis8 g4 a8[ b] |
  a4 g fis d |
  g g d' a |
  b2. \bar""\break e4 |
  
  d e b a8[ g] |
  c4 b a g |
  fis e b' b |
  \partial 2. e,2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8[ e] |
  fis4 e d c8[ b] |
  e4 d8[ e] fis4 e |
  c e e dis |
  
  e2. g4 |
  fis e d c8[ b] |
  e4 d8[ e] fis4 e |
  c e e dis |
  e2. \bar "||"
  
  dis!4 |
  e4. dis8 e4 fis |
  e cis d d |
  d g g fis |
  g2. e4 |
  
  a g fis e |
  e8[ fis] g4 fis e |
  c e e dis |
  e2. \bar "||"
  
  dis!4 |
  e4. dis8 e4 fis |
  e cis d d |
  d g g fis |
  g2. e4 |
  
  a g fis e |
  e8[ fis] g4 fis e |
  c e e dis |
  e2. \bar ":|."
  
  
  
  
  d8[ e] |
  fis4 e d c8[ b] |
  e4 d8[ e] fis4 e |
  c e e dis |
  
  e2. g4 |
  fis e d c8[ b] |
  e4 d8[ e] fis4 e |
  c e e dis |
  e2. \bar "||"
  
  dis!4 |
  e4. dis8 e4 fis |
  e cis d d |
  d g g fis |
  g2. e4 |
  
  a g fis e |
  e8[ fis] g4 fis e |
  c e e dis |
  e2. \bar "|."
}
altoWords = {
  \dropLyricsVII
  \lyricmode {
    \set stanza = #"1. "
    The Lord at first had A -- dam made Out of the dust and clay,
    And in his nos -- trils breath -- ed life, E’en as the Scrip -- tures say.
    
    And then in E -- den’s Pa -- ra -- dise He pla -- ced him to dwell,
    That he with -- in it should re -- main, To dress and keep it well.
  }
  \set stanza = \markup\dynamic"ff "
  \lyricmode{
    Now let good Chris -- tians all be -- gin A ho -- lier life to live,
    And to re -- joice and mer -- ry be, For this is Christ -- mas Eve.

    \set stanza = #"4."
    Now mark the good -- ness of the Lord, Which He to man -- kind bore;
    His mer -- cy soon He did ex -- tend, Lost man for to re -- store:
    And there -- fore to re -- deem our souls From death and hell and thrall,
    He said His own dear Son should be The Sav -- ior of us all.
  }
}
altoWordsII = {
  \dropLyricsVII
  \set stanza = \markup{\dynamic"mf  " "2. "}
  \lyricmode {
  %\markup\italic
    And thus with -- in the gar -- den he Was set, there -- in to stay;
    And in com -- mand -- ment un -- to him
    These words the Lord did say:
  }
  \set stanza = \markup\dynamic"p "
  \lyricmode{
    “The fruit which in the gar -- den grows To thee shall be for meat,
    Ex -- cept the tree in midst there -- of,
    Of which thou shalt not eat.”
    
    \repeat unfold 28 {\skip1}
  }
  \set stanza = \markup{\dynamic" mf " "5."}
  \lyricmode {
    Which prom -- ise now is brought to pass: Chris -- tians, be -- lieve it well:
    And by the death of God’s dear Son, We are re -- deemed from Hell.
  }
  \set stanza = \markup\dynamic"p "
  \lyricmode{
    So if we tru -- ly do be -- lieve, And do the thing that’s right,
    Then by His mer -- its we at last Shall live in heav -- en bright.
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"3. "
  “For in the day thou shalt it touch Or dost to it come nigh,
  If so thou do but eat there -- of, Then thou shalt sure -- ly die.”
  But A -- dam he did take no heed Un -- to that on -- ly thing,
  But did trans -- gress God’s ho -- ly Law, And so was wrapt in sin.
  
  \repeat unfold 28 {\skip1}
  \set stanza = #"6."
  And now the tide is nigh at hand, In which our Sav -- ior came;
  Let us re -- joice and mer -- ry be In keep -- ing of the same;
  Let’s feed the poor and hun -- gry souls. And such as do it crave;
  And when we die, in heav -- en we Our sure re -- ward shall have.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  g4 |
  a g8[ a] b4 fis8[ g] |
  g[ a] b[ c] d4 b |
  a g g fis |
  
  g2. g4 |
  a g8[ a] b4 fis8[ g] |
  g[ a] b[ c] d4 b |
  a g g fis |
  g2. \bar "||"
  
  b4 |
  b4. b8 b4 d |
  a a a fis |
  d' c b d |
  d2. b4 |
  
  a b b b |
  c d d b |
  a g g fis |
  g2. \bar "||"
  
  b4 |
  b4. b8 b4 d |
  a a a fis |
  d' c b d |
  d2. b4 |
  
  a b b b |
  c d d b |
  a g g fis |
  g2. \bar ":|."
  
  
  
  g4 |
  a g8[ a] b4 fis8[ g] |
  g[ a] b[ c] d4 b |
  a g g fis |
  
  g2. g4 |
  a g8[ a] b4 fis8[ g] |
  g[ a] b[ c] d4 b |
  a g g fis |
  g2. \bar "||"
  
  b4 |
  b4. b8 b4 d |
  a a a fis |
  d' c b d |
  d2. b4 |
  
  a b b b |
  c d d b |
  a g g fis |
  g2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  g4 |
  d e8[ fis] g4 d8[ e] |
  c4 g' d e |
  a, c b b |
  
  e2. e4 |
  d e8[ fis] g4 d8[ e] |
  c4 g' d e |
  a, c b b |
  e2. \bar "||"
  
  b4 |
  e4. b8 e4 d |
  cis a d c |
  b e d d |
  g2. g4 |
  
  fis e dis e |
  a g d e |
  a, c b b |
  e2. \bar "||"
  
  b4 |
  e4. b8 e4 d |
  cis a d c |
  b e d d |
  g2. g4 |
  
  fis e dis e |
  a g d e |
  a, c b b |
  e2. \bar ":|."
  
  
  
  
  
  g4 |
  d e8[ fis] g4 d8[ e] |
  c4 g' d e |
  a, c b b |
  
  e2. e4 |
  d e8[ fis] g4 d8[ e] |
  c4 g' d e |
  a, c b b |
  e2. \bar "||"
  
  b4 |
  e4. b8 e4 d |
  cis a d c |
  b e d d |
  g2. g4 |
  
  fis e dis e |
  a g d e |
  a, c b b |
  e2. \bar "|."
}
bassWords = \lyricmode {

}

pianoRH = \relative c' {
  
}
pianoLH = \relative c' {
  
}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women"} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women"} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women"} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women"} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women"} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women"} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
  \context {
    \Lyrics
    \override LyricText.font-size = #1.3
  }
    \context {
      \Score
      %\override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/8)
      %\override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

