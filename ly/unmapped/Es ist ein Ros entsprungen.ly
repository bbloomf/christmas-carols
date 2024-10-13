\version "2.14.2"
\include "util.ly"
\header {
  tagline = ""%\markup { "from" \concat{\italic "HymnsAndCarolsOfChristmas.com"}}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Es ist ein Ros entsprungen"}}
    poet = \markup\oldStyleNum"15th Century German"
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
       (padding . 12)
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
global = {
  \key g \major
  \time 4/4
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
  \autoBeamOff
}

sopMusic = \relative c'' {
  \partial 2
  \repeat unfold 2 {
    d2 |
    d4 d e d |
    d2 b |
    c b4 a~ |
    a g2 fis4 |
    g2
  }
  b4\rest b4 |
  a fis g e |
  d2 b'4\rest d4 |
  d d e d |
  d2 b |
  c b4\! a~ |
  a g2 fis4 |
  g2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  \repeat unfold 2 {
    b2 |
    b4 g g g |
    fis2 e |
    e d4 d |
    e4.( b8 d4) d4 |
    d2
  }
  s4 g |
  e d d cis |
  d8[ e]( fis4) s fis |
  a g g g |
  fis2 e |
  e g4 e |
  fis4( g a) d, |
  d2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Es ist ein Ros ent -- sprung -- en,
  aus ein -- er Wur -- zel zart,
  wie uns die Alt -- en sung -- en,
  von Jes -- se kam __ die Art
  \set associatedVoice = "tenors"
  Und hat ein Blüm -- lein bracht
  \unset associatedVoice
  mit -- ten im kalt -- en Win -- ter,
  wohl zu der hal -- ben Nacht.
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  Das Rös -- lein, das ich mein -- e,
  da -- von Je -- sai -- a sagt,
  ist Ma -- ri -- a die rei -- ne
  die uns das Blüm -- lein bracht.
  \set associatedVoice = "tenors"
  Aus Got -- tes ew’ -- gem Rat
  \unset associatedVoice
  hat sie ein Kind ge -- bor -- en
  und blieb ein rei -- ne Magd.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Das Blüm -- e -- lein, so klein -- e,
  das duf -- tet uns __ so süß,
  mit sein -- em hel -- len Schein -- e
  ver -- treibt’s die Fin -- ster -- nis.
  \set associatedVoice = "tenors"
  Wahr Mensch und wahr -- er Gott,
  \unset associatedVoice
  hilft uns aus al -- lem Leid -- e,
  ret -- tet von Sünd und Tod.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  \repeat unfold 2 {
    d2 |
    d4 b c b |
    a2 g |
    g b4 d |
    c( b2) a4 |
    b2
  }
  s4 d |
  c b b a |
  fis8[ g]( a4) s4 a4 |

  a b c b |
  a2 gis |
  a d4 c |
  b2 a |
  b \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \repeat unfold 2 {
    g'2 |
    g4 g c g |
    d2 e |
    c g'4 fis |
    e2 d |
    g,
  }
  d'4\rest g |
  a b g a |
  d,2 d4\rest d |
  fis g c, g' |
  d2 e |
  a, b4 c |
  d2 d |
  g, \bar "|."
}
bassWords = \lyricmode {
  \repeat unfold 11 { \skip 1 }
  -zel zart,
  \repeat unfold 11 { \skip 1 }
  die Art
  \repeat unfold 17 { \skip 1 }
  -en Nacht.
}
bassWordsII = \lyricmode {
  \repeat unfold 11 { \skip 1 }
  -a sagt,
  \repeat unfold 11 { \skip 1 }
  -lein bracht.
  \repeat unfold 17 { \skip 1 }
  -ne Magd.
}
bassWordsIII = \lyricmode {
  \repeat unfold 11 { \skip 1 }
  so süß,
  \repeat unfold 11 { \skip 1 }
  -ster -- nis.
  \repeat unfold 17 { \skip 1 }
  und Tod.
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "altos" \altoWords
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . 0.2)) } \lyricsto "tenors" \tenorWords
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
