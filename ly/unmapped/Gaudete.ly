\version "2.14.2"
\include "util.ly"
\header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"Gaudete"}}
    composer = \markup\oldStyleNum"15th Century"
    tagline = \markup\concat{"Chorus and text of verses from " \italic"Piæ Cantiones" \oldStyleNum", 1582, via " \italic"imslp.org" ", Melody of verses from " \italic "www.cpdl.org"}
  }\paper {
  print-all-headers = ##f
  paper-height = 9\in
  paper-width = 6\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #194
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
  \key f\major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c'' {
  \partial 4 g4 |
  g f g8 a bes4 |
  bes8 a4 g8 f4 f |
  
  f g a4. g8 |
  f4 g8 a4 g8 f4 |
  \partial 2. g2. \bar "||" \break
}

verseMusic = \relative c'' {
  
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 |
  d d d8 f f4 |
  f8 f4 d8 d4 d |
  
  d d f4. d8 |
  d4 bes8 f'4 c8 d4 |
  d2.
  
  \slurDotted \oneVoice
  g8 d' d c bes( a) g4 |
  a8 bes a g f4 d |
  
  g8 bes a bes c( a) f4 |
  bes8 g f a g4 g \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  Gau -- de -- te, gau -- de -- te,
  Chris -- tus est na -- tus
  ex Ma -- ri -- a Vir -- gi -- ne, Gau -- de -- te!
  
  \dropLyricsIV
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Tem -- pus ad -- est gra -- ti -- æ
  hoc quod op -- ta -- ba -- mus,
  Car -- mi -- na læ -- ti -- ti -- æ
  de -- vo -- te red -- da -- mus.
}
altoWordsII = \lyricmode {
  \dropLyricsIV
%\markup\italic
  \repeat unfold 21 {\skip1}
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  De -- us ho -- mo fac -- tus est
  na -- tu -- ra mi -- ran -- te,
  Mun -- dus re -- no -- va -- tus est
  a Chris -- to re -- gnan -- te.
}
altoWordsIII = \lyricmode {
  \dropLyricsIV
  \repeat unfold 21 {\skip1}
  \set stanza = #"3. "
  E -- ze -- chie -- lis por -- ta
  clau -- sa per -- tran -- si -- tur,
  Un -- de lux est or -- ta,
  sa -- lus in -- ve -- ni -- tur.
}
altoWordsIV = \lyricmode {
  \dropLyricsIV
  \repeat unfold 21 {\skip1}
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  Er -- go nos -- tra con -- ti -- o
  psal -- lat jam in lu -- stro,
  Be -- ne -- di -- cat Do -- mi -- no,
  sa -- lus Re -- gi nos -- tro.
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
  g4 |
  bes a bes8 c d4 |
  d8 c4 bes8 a4 a |
  
  a bes c4. bes8 |
  a4 g8 f4 g8 a4 |
  <g d'>2.
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'4 |
  g d g8 f bes,4 |
  bes8 f'4 g8 d4 d |
  
  d g f4. g8 |
  d4 e8 f4 e8 d4 |
  g,2.
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
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
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
%6x9 \context {\Lyrics\override LyricText #'font-size = #1.1 }
%6.14 \context {\Lyrics\override LyricText #'font-size = #0.6 }
    #(define fonts (make-pango-font-tree "GoudyOlSt BT" "Garamond Premier Pro" "Garamond Premier Pro" (/ 14.7 20)))
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
    \tempo 4 = 135
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
