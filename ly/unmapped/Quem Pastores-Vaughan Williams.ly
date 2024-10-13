\version "2.14.2"
\include "util.ly"
\header {
    tagline = \markup { "from" \italic {ChristmasCarolMusic.org}}

    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Quem Pastores"}}
%    poet = \markup\oldStyleNum"Anonymous, 14th Century"
%    composer = \markup\oldStyleNum"14th Century German"
    composer = \markup\oldStyleNum"Arranged by Ralph Vaughan Williams (1872–1958)"
    tagline = \markup { "from" \italic {ChristmasCarolMusic.org}}
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
       (padding . -15)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #163
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

\markup \fill-line {\center-column{
  \concat{ "Music from " \italic "The Cowley Carol Book" \oldStyleNum", 1919, Words from " \italic "HymnsAndCarolsOfChristmas.com"}}}
\markup\vspace#1



















global = {
  \key f \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  f2 a4 |
  c2 a4 |
  bes( c) d |
  c2 g4 |
  a( bes) c |
  
  bes( a) g |
  f2 d4 |
  e2 c4 |
  a'2 bes4 |
  c2 d4 |
  
  c2 g4 |
  a2 f4 |
  bes2 bes4 |
  a( g) f |
  f( d) e |
  f2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  f2 f4 |
  g2 f4 |
  f2 f4 |
  f2 e4 |
  f2 f4 |
  
  d2 e4 |
  d2 b4 |
  c2 c4 |
  c2 f4 |
  f2 f4 |
  
  f2 e4 |
  e2 d4 |
  d2 g4 |
  e2 d4 |
  d2 c4 |
  c2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  Quem pas -- to -- res lau -- da -- ve -- re,
  Qui -- bus an -- ge -- li di -- xe -- re,
  “Ab -- sit vo -- bis jam ti -- me -- re,
  Na -- tus est rex glo -- ri -- æ.”
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  Ad quem ma -- gi am -- bu -- la -- bant,
  Au -- rum, thus, myr -- rham por -- ta -- bant,
  Im -- mo -- la -- bant hæc sin -- ce -- re
  Le -- o -- ni vic -- to -- ri -- æ.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  Ex -- ul -- te -- mus cum Ma -- ri -- a
  In cæ -- les -- ti hie -- rar -- chi -- a
  Na -- tum pro -- mat vo -- ce pi -- a
  Laus, ho -- nor et glo -- ri -- a.
}
altoWordsIV = \lyricmode {
  \dropLyricsIX
  \set stanza = #"4. "
  Chris -- to re -- gi, De -- o na -- to,
  Per Ma -- ri -- am no -- bis da -- to,
  Me -- ri -- to re -- so -- net ve -- re
  Dul -- ci cum me -- lo -- di -- a.
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  a2 c4 |
  c2 c4 |
  bes2 bes4 |
  g( a) bes |
  a2 a4 |
  
  bes2 bes4 |
  a2 g4 |
  g2 e4 |
  f2 f4 |
  f2 bes4 |
  
  g( a) bes |
  a2 a4 |
  bes2 d4 |
  c2 a4 |
  bes2 bes4 |
  a2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f2 f4 |
  e2 f4 |
  d2 bes4 |
  c2 c4 |
  f2 a4 |
  
  g2 c,4 |
  d2 g4 |
  c,2 c4 |
  f2 d4 |
  a2 bes4 |
  
  c2 c4 |
  cis2 d4 |
  g,2 g4 |
  a2 d4 |
  g,2 c4 |
  f2. \bar "|."
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
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
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
    \tempo 4 = 120
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
