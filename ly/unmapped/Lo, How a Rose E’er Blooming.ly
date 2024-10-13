\version "2.14.2"
\include "util.ly"
\header {
  tagline = ""%\markup { "from" \concat{\italic "HymnsAndCarolsOfChristmas.com"}}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Lo, How a Rose E’er Blooming"}}
    poet = \markup\oldStyleNum"15th Century German"
    meter = \markup\oldStyleNum"Translated by Theodore Baker (1851–1934)"
    composer = \markup\oldStyleNum"Arranged by Michael Praetorius (1571–1621)"
    tagline = \markup { "from" \concat{\italic "Christmas Carols and Hymns for School and Choir" \oldStyleNum", 1910"}}
  }
\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -12)
       (stretchability . 100))
  %markup-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -5)
  %     (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -2)
       (stretchability . 100))
%{IF_LESSER
  markup-system-spacing #'stretchability = 50
  top-markup-spacing #'stretchability = 30
  last-bottom-spacing #'stretchability = 60
%}%END_IF_LESSER
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #092
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

%END_IF_NOT_LESSER







global = {
  \key g \major
  \time 4/4
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  d2 d4 d |
  e d d2 |
  b c |
  b4 a2 g4( |
  
  g) fis g2 |
  d' d4 d |
  e d d2 |
  b c |
  
  b4 a2 g4( |
  g) fis g2 |
  b4\rest b4 a fis |
  g e d2 |
  
  b'4\rest d4 d d |
  e d d2 |
  b c |
  b4 a2 g4( |
  g) fis g2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  b2 b4 g |
  g g fis2 |
  e e |
  d4 d e4.( b8 |
  
  d4) d4 d2 |
  b'2 b4 g |
  g g fis2 |
  e e |
  
  d4 d e4.( b8 |
  d4) d d2 |
  s4 g e d |
  d cis d2 |
  
  s4 fis a g |
  g g fis2 |
  e e |
  g4 e fis2( |
  d4) d d2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Lo, how a Rose e’er bloom -- ing
  From ten -- der stem __ has sprung!
  Of Jes -- se’s lin -- eage com -- ing
  As men of old __ have sung.
  It came, a flow’r -- et bright,
  A -- mid the cold of win -- ter
  When half -- spent was __ the night.
}
altoWordsII = {
  \dropLyricsV
  \set stanza = \markup{"2. "}
  \lyricmode {
    I -- sa -- iah ’twas fore -- told it,
    The Rose I had __ in mind;
    With Ma -- ry we be -- hold it,
    The Vir -- gin Moth -- er kind.
    To show God’s love a -- right
    She bore to men a Sav -- ior,
    When half -- spent was __ the night.
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  O Flow’r, whose fra -- grance ten -- der
  With sweet -- ness fills __ the air,
  Dis -- pel with glo -- rious splen -- dor
  The dark -- ness ev -- ’ry -- where;
  True man, yet ve -- ry God,
  From Sin and death now save us,
  And share our ev -- ’ry load.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  d2 d4 b |
  c b a2 |
  g g |
  b4 d c( b~ |
  
  b) a b2 |
  d d4 b |
  c b a2 |
  g g |
  
  b4 d c( b~ |
  b) a b2 |
  s4 d4 c b |
  b a fis8[( g] a4) |
  
  s4 a4 a b |
  c b a2 |
  gis a |
  d4 c b2( |
  b4) a b2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'2 g4 g |
  c g d2 |
  e c |
  g'4 fis e2 |
  
  d2 g |
  g g4 g |
  c g d2 |
  e c |
  
  g'4 fis e2 |
  d g, |
  d'4\rest g a b |
  g a d,8[( e] fis4) |
  
  d4\rest d fis g |
  c, g' d2 |
  e a, |
  b4 c d2 |
  d g \bar "|."
}
bassWords = \lyricmode {
  \repeat unfold 11 { \skip 1 }
  has sprung!
  \repeat unfold 11 { \skip 1 }
  have sung.
  \repeat unfold 17 { \skip 1 }
  the night.
}
bassWordsII = \lyricmode {
  \repeat unfold 11 { \skip 1 }
  in mind;
  \repeat unfold 11 { \skip 1 }
  -er kind.
%{IF_LESSER
  \repeat unfold 17 { \skip 1 }
  the night.
%}%END_IF_LESSER
}
bassWordsIII = \lyricmode {
  \repeat unfold 11 { \skip 1 }
  the air,
  \repeat unfold 11 { \skip 1 }
  ’ry -- where;
  \repeat unfold 17 { \skip 1 }
  ’ry load.
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
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "tenors" \tenorWords
%{IF_LESSER
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWordsIII
%}%END_IF_LESSER
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWordsII
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)(padding . 0.2)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \layout {
    %#(layout-set-staff-size 15)
    %#(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 15 20)))
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
