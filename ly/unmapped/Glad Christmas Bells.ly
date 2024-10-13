\version "2.14.2"
\include "util.ly"
\header {
  tagline = \markup\concat{ "from " \italic\oldStyleNum"Franklin Square Song Collection, No. 1" \oldStyleNum", 1881, via " \italic"HymnsAndCarolsOfChristmas.com"}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Glad Christmas Bells"}}
    poet = \markup\oldStyleNum"Anonymous"
    composer = \markup\oldStyleNum"Anonymous"
    tagline = \markup\concat{ "from " \italic\oldStyleNum"Franklin Square Song Collection, No. 1" \oldStyleNum", 1881, via " \italic"HymnsAndCarolsOfChristmas.com"}
  }
\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #104
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine 
        \fill-line{"" \if \should-print-page-number
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #8.5
     \combine
        \if \should-print-page-number
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 15) \paper{ #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20))) }
global = {
  \key g \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \repeat volta 5 {
    \partial 4 d8[ g] |
    g8 fis fis4 fis8[ a] |
    a g g4 \bar"||" g8[ b] |
    b4. a8 c b |
    \partial 2 a4 g\fermata \bar"||"
    
    \partial 4 d8[ g] |
    g fis fis4 fis8[ a] |
    a g g4 \bar"||" b8[ e] |
    d4. g,8 a b |
    \partial 2 a4 g\fermata
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 |
  d8 d d4 d |
  d8 d d4 b8[ d] |
  e4. e8 e e |
  fis4 g |
  
  d |
  d8 d d4 d |
  d8 d d4 g |
  g4. g8 g g |
  fis4 g
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  Glad Christ -- mas bells, your mu -- sic tells
    The sweet and pleas -- ant sto -- ry;
  How came to earth, in __ low -- ly birth,
    The Lord of life and glo -- ry.
    
  \set stanza = #"6. "
  “Fear not,” said he, for __ trem -- bling -- ly
    The shep -- herds stood in won -- der,
  “Glad news I bring, the prom -- ised King
    Lies in a sta -- ble yon -- der.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"2. "
  No pal -- ace hall its __ ceil -- ing tall
    His king -- ly head spread o -- ver,
  There on -- ly stood a __ sta -- ble rude
    The heav’n -- ly Babe to cov -- er.
  
  \set stanza = #"7. "
  “And by this sign, the __ Babe Di -- vine
    You may dis -- cov -- er sure -- ly,
  A __ man -- ger rude His dwell -- ing is,
    There lies He, cra -- dled poor -- ly.”
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Nor rai -- ment gay, as __ there He lay,
    A -- dorn’d the in -- fant Stran -- ger;
  Poor, hum -- ble Child of __ moth -- er mild,
    She laid Him in a man -- ger.
  
  \set stanza = #"8. "
  Then swift -- ly came, in __ lines of flame,
    Like count -- less me -- teors blaz -- ing,
  A __ mul -- ti -- tude, and with Him stood,
    A __ spec -- ta -- cle a -- maz -- ing.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  But from a -- far, a __ splen -- did star
    The wise men west -- ward turn -- ing;
  The live -- long night saw pure and bright,
    A -- bove His birth -- place burn -- ing.
  
  \set stanza = #"9. "
  And all the choir, with tongues of fire
    Broke forth in joy -- ful sing -- ing,
  Till __ with their cry the ve -- ry sky
    From end to end was ring -- ing.
}
altoWordsV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"5. "
  Where on the hill, all __ safe and still,
    The fold -- ed flocks were ly -- ing,
  Down through the air an __ an -- gel fair
    On wing of flame came fly -- ing.
  
  \set stanza = #"10. "
  “Glo -- ry to Thee for -- ev -- er be,
    God in the high -- est, glo -- ry!
  Good will to men, and peace a -- gain
    O __ earth is beam -- ing o’er Thee!”
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  b4 |
  b8 a a4 a8[ c] |
  c b b4 g |
  g4. g8 a g |
  c4 b |
  
  b |
  b8 a a4 a8[ c] |
  c b b4 d8[ c] |
  b4. b8 c d |
  c4 b
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'4 |
  g8 d d4 d |
  d8 g g4 g |
  c,4. c8 a a |
  d4 g |
  
  g |
  d8 d d4 d |
  d8 g g4 g |
  g4. g8 g g |
  d4 g
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold2\sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold2\altoMusic >> }
    >>
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold2\tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold2\bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      %\override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText #'X-offset = #center-on-word
    }
  }
  
  \midi {
    \tempo 4 = 100
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
