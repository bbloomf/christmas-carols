\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Angel Gabriel"}}
  poet = \markup\oldStyleNum"Translated and Adapted by Sabine Baring-Gould (1834–1924)"
  composer = \markup\oldStyleNum"Basque Carol"
  tagline = \markup { "from" \italic {CyberHymnal.org}}
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  %system-system-spacing =
  %  #'((basic-distance . 0)
  %     (minimum-distance . 0)
  %     (padding . -0.35)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #049
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
  \key bes \major
  \time 6/4
}

sopMusic = \relative c' {
  \partial 8 d8 |
  \partial 8*9 g4 bes8 a4 c8 bes4 a8 |
  g4. a d,~ d4 \bar""\break d8 |
  
  \partial 8*9 g4 bes8 a4 c8 bes4 a8 |
  g4.~ g4 f8 g4.~ g4 \bar""\break bes8 |
  
  bes4 c8 bes4 a8 bes4 c8 d4 d8 |
  \slurDashed c4.( bes) \slurSolid a4.~ a4 \bar""\break bes8 |
  
  c4 bes8 a4 g8 a4. d, |
  g4.( bes8[ a bes] g4.~ g4) f8 |
  \partial 8*11 g2.~ g4. bes4\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  bes8 |
  d4 d8 d4 d8 d4 d8 |
  ees4. ees a,~ a4 d8 |
  
  d4 d8 d4 d8 d4 d8 |
  ees4.( d4) d8 d4.~ d4 d8 |
  
  d4 d8 d4 d8 d4 f8 f4 f8 |
  \slurDashed f4.( d) \slurSolid d4.~ d4 d8 |
  
  ees4 ees8 ees4 ees8 d4. d4. |
  d4.( ees ees d4) d8 |
  d2.~ d4. s4 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  The an -- gel Ga -- bri -- el from heav -- en came, __
  His wings as drif -- ted snow, his eyes __ a -- flame; __
  “All hail,” said he, “thou low -- ly maid -- en Ma -- ry, __
  Most high -- ly fa -- vored la -- dy,”
  \markup\italic Gló -- \markup\italic ri -- \markup\italic a! __
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  “For know a bles -- sed Mo -- ther thou shalt be, __
  All ge -- ne -- ra -- tions laud and hon -- or thee, __
  Thy Son shall be Em -- man -- u -- el, by
  \set ignoreMelismata = ##t
  seers fore -- 
  \unset ignoreMelismata
  told, __
  Most high -- ly fa -- vored la -- dy,”
  \markup\italic Gló -- \markup\italic ri -- \markup\italic a! __
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Then gen -- tle Ma -- ry meek -- ly bowed her head, __
  “To me be as it plea -- seth God,” __ she said, __
  “My soul shall laud and mag -- ni -- fy His
  \set ignoreMelismata = ##t
  ho -- ly
  \unset ignoreMelismata
  Name.”
__
  Most high -- ly fa -- vored la -- dy,
  \markup\italic Gló -- \markup\italic ri -- \markup\italic a! __
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  Of her, Em -- man -- u -- el, the Christ, was born __
  In Beth -- le -- hem, all on a Christ -- mas morn, __
  And Chris -- tian folk through -- out the world will
  \set ignoreMelismata = ##t
  ev -- er
  \unset ignoreMelismata
  say, __
  “Most high -- ly fa -- vored la -- dy,”
  \markup\italic Gló -- \markup\italic ri -- \markup\italic a! __
}

tenorMusic = \relative c' {
  g8 |
  bes4 bes8 a4 a8 bes4 bes8 |
  bes4. g g( fis4) a8 |
  
  bes4 bes8 a4 a8 bes4 bes8 |
  bes4.( a4) a8 bes4.~ bes4 bes8 |
  
  bes4 a8 bes4 c8 d4 c8 bes4 bes8 |
  \slurDashed a4.( g) \slurSolid a~ a4 a8 |
  
  g4 g8 c4 bes8 a4. a |
  bes4.( bes g a4) a8 |
  g2.~ g4. d4\rest \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  g8 |
  g4 g8 d4 d8 g4 g8 |
  ees4. c d~ d4 d8 |
  
  g4 g8 d4 d8 g4 g8 |
  ees4.( d4) d8 g4.~ g4 g8 |
  
  g4 d8 g4 fis8 g4 a8 bes4 bes8 |
  \slurDashed f4.( g) \slurSolid d4.~ d4 d8 |
  
  c4 c8 ees4 ees8 d4. d |
  g( ees c d4) d8 |
  g,2.~ g4. s4 \bar "|."
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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

