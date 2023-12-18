\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Coventry Carol"}}
  poet = \markup\oldStyleNum"Robert Croo, 1534"
  composer = \markup\oldStyleNum"16th Centry English Carol"
  arranger = \markup\oldStyleNum"Adapted and Arranged by Sir John Stainer (1840–1901)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
}
\paper {
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
  first-page-number = #062
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
  \time 3/4
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  <g d>4^\p <g d> <fis d> |
  <g d>2 <bes d,>4 |
  <a f>2 <g d>4 |
  <fis d>2. |
  <g d>4 <fis d> <g bes,> | \break
  <c ees,>2 <a d,>4 | 
  <b d,>2. \bar "||" 
  
  g4 g fis |
  g2 bes4 |
  a2 g4 |
  fis2. | \break
  
  g4 a bes |
  c2 a4 |
  g2.~ |
  <<g2 {s4 s^\mf}>> d'4 |
  c2 bes4 |
  a2 bes4 | \break
  
  a2^\markup\italic"dim." g4 |
  fis2. |
  <<g4 {s8^\pp s^\markup\italic"rall."}>> fis4 g |
  c2 a4 |
  b2.~ |
  b2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  s2.*7 |
  
  d4 d d |
  d2 d4 |
  f2 d4 |
  d2. |
  
  d4 d d |
  ees2 d4 |
  d2.~ |
  d2 f4 |
  f2 f4 |
  f2 g4 |
  
  ees2 c4 |
  d2. |
  d4 d bes |
  ees2 d4 |
  d2.~ |
  d2. \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsXI
  \repeat unfold 14 \skip 1
  \set stanza = #"1. "
  Lul -- lay, Thou lit -- tle ti -- ny Child,
  By, by, lul -- ly, lul -- lay; __
  Lul -- lay, Thou lit -- tle ti -- ny child,
  By, by, lul -- ly, lul -- lay. __
}
altoWordsII = \lyricmode {
  \dropLyricsXI
  \repeat unfold 14 \skip 1
  \set stanza = #"2. "
  O sis -- ters too, how may we do,
  For to pre -- serve this day; __
  This poor Young -- ling for whom we sing,
  By, by, lul -- ly, lul -- lay? __
}
altoWordsIII = \lyricmode {
  \dropLyricsXI
  \repeat unfold 14 \skip 1
  \set stanza = #"3. "
  Her -- od, the king, in his rag -- ing,
  Charg -- ed he hath this day; __
  His men of might, in his own sight,
  All chil -- dren young to slay. __
}
altoWordsIV = \lyricmode {
  \dropLyricsXI
  \repeat unfold 14 \skip 1
  \set stanza = #"4. "
  Then woe is me, poor Child, for Thee,
  And ev -- er mourn and say; __
  For Thy part -- ing nor say nor sing,
  By, by, lul -- ly, lul -- lay. __
}

tenorMusic = \relative c' {
  s2.*6_\p |
  
  <g g,>2. ~ |
  
  bes4 bes a |
  bes2 bes4 |
  c2 bes4 |
  a2. |
  
  bes4 a g |
  g2 fis4 |
  bes2.~ |
  <<bes2 {s4 s_\mf}>> bes4 |
  a2 bes4 |
  c2 d4 |
  
  c2_\markup\italic"dim." g4 |
  a2. |
  <<bes4 {s8_\pp s_\markup\italic"rall."}>> a4 g |
  g2 fis4 |
  g2.~ |
  g2. \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  <bes g>4 q <a d,> |
  <bes g>2 q4 |
  <c f,>2 <bes g>4 |
  <a d,>2. |
  <bes g>4 <a d,> <g ees> |
  <g c,>2 <fis d>4 |
  
  s2. |
  g4 g d |
  g2 g4 |
  f2 g4 |
  d2. |
  
  g4 fis g |
  c,2 d4 |
  g2.~ |
  g2 bes,4 |
  f'2 d4 |
  f2 bes,4 |
  
  c2 ees4 |
  d2. |
  g4 d ees |
  c2 d4 |
  g,2.~ |
  g2. \bar "|."
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
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

