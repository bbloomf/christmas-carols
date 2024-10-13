\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #15 \smallCapsOldStyle"The Coventry Carol"}}
  poet = \markup\oldStyleNum"Robert Croo, 1534"
  composer = \markup\oldStyleNum"16th Centry English Carol"
  arranger = \markup\oldStyleNum"Arranged by Martin Fallas Shaw (1875–1958)"
  tagline = \markup { "from" \italic {ChristmasCarolMusic.org}}
}
\paper {
  paper-height = 9\in
  paper-width = 6\in
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
  inner-margin = 0.5\in
  outer-margin = 0.25\in
  top-margin = 0.25\in
  bottom-margin = 0.25\in
  first-page-number = #063
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
  \key bes \major
  \time 3/4
}

sopMusic = \relative c'' {
  g4 g fis |
  g2 bes4 |
  a8 a a4 g |
  fis2. |
  g4 a bes |
  c a2 |
  b2. \bar "||" \break
  
  g4 g fis |
  g2 bes4 |
  a2 g4 |
  fis2. |
  g4 a bes | \break
  c a2 |
  g2 d'4 |
  
  c2 bes4 |
  a2 bes4 |
  a2 g4 | \break
  fis2. |
  g4 fis g |
  c a2 |
  b2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 d d |
  d2 g4 |
  f8 f f4 d |
  d2. |
  d4 f f |
  g2 fis4 |
  g2. |
  
  d4 d d |
  d2 g4 |
  f2 d4 |
  d2. |
  d4 f f |
  g2 fis4 |
  g2 f4 |
  
  a2 g4 |
  f2 d4 |
  f2 c4 |
  d2. |
  d4 d d |
  g2 fis4 |
  g2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsXI
  Lul -- ly, lul -- lay, Thou lit -- tle ti -- ny Child,
  By, by, lul -- ly, lul -- lay;
  
  \set stanza = #"1. "
  O sis -- ters too, how may we do,
  For to pre -- serve this day;
  This poor Young -- ling for whom we sing
  By, by, lul -- ly, lul -- lay?
}
altoWordsII = \lyricmode {
  \dropLyricsXI
  \repeat unfold 16 { \skip 1 }
  \set stanza = #"2. "
  Her -- od, the king, in his rag -- ing,
  Charg -- ed he hath this day;
  His men of might, in his own sight,
  All chil -- dren young to slay.
}
altoWordsIII = \lyricmode {
  \dropLyricsXI
  \repeat unfold 16 { \skip 1 }
  \set stanza = #"3. "
  Then woe is me, poor Child, for Thee!
  And ev -- er mourn and say;
  For Thy part -- ing nor say nor sing,
  By, by, lul -- ly, lul -- lay.
}
altoWordsIV = \lyricmode {
  \repeat unfold 2 { \skip 1 }
  \set stanza = #"4. "
  
}

tenorMusic = \relative c' {
  bes4 bes a |
  bes2 ees4 |
  c8 c c4 bes |
  a2. |
  bes4 c d |
  ees4 d2 |
  d2. |
  
  bes4 bes a |
  bes2 ees4 |
  c2 bes4 |
  a2. |
  bes4 c d |
  ees4 d2 |
  d d4 |
  
  f2 d4 |
  d2 bes4 |
  c2 g4 |
  a2. |
  bes4 a bes |
  ees d2 |
  d2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  g4 g d |
  g2 ees4 |
  f8 f f4 g |
  d2. |
  g4 f d |
  c d2 |
  g,2. |
  
  g'4 g d |
  g2 ees4 |
  f2 g4 |
  d2. |
  g4 f d |
  c d2 |
  g bes4 |
  
  f2 g4 |
  d2 g4 |
  f2 ees4 |
  d2. |
  g4 d g |
  c, d2 |
  g2. \bar "|."
}
bassWords = \lyricmode {

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
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
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
    \tempo 4 = 90
    \set Staff.midiInstrument = "piccolo"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}
