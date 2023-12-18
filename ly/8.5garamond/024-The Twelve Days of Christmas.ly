\version "2.24.0"
\include "util.ly"
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Twelve Days of Christmas"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"Traditional"
  %arranger = \markup\oldStyleNum"Edited by Benjamin Bloomfield (1984–)"
  tagline = ""
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
  first-page-number = #024
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
global = {
  \key g \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \partial 4 d8 d |
  d4 g8 g g4 fis8 g |
  a b c a b4. c8 |
  
  d4 e8[ c] b g a4 | \break
  g2~ g8 b\rest \bar "||"
  d,8 d |
  d8 g g g g4 fis8 g |
  
  a b c a b2 | \break
  d4 a8 b c4 b8 c |
  d4 e8[ c] b g a4 |
  g2~g8 b\rest | \break
  
  \repeat volta 2 {
    d,8 d |
    d4 g8 g g4 fis8 g |
    a b c a b2 |
    \time 3/4 d4 a8 b c4 |
    d4 a8[ b] c4 |
    \time 4/4
    
    d4 a8 b c4 b8 c |
    d4 e8[ c] b g a4 |
    g2~g8 b8\rest |
  }
  \break
  
  %5th verse
    \repeat volta 2 {
    \partial 4 d,8 d |
    d4 g8 g g4 fis8 g |
    a b c a b2 | \break
    \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
    \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
    \mark\markup\italic"skip to appropriate measure"

    
    \time 3/4
    d4 a8 b c a |
    \mark\markup\bold"11."
    d4 a8 b c a |
    \mark\markup\bold"10."
    d4 a8 b c a |
    \mark\markup\bold"9."
    d4 a8 b c a |
    \mark\markup\bold"8."
    d4 a8 b c a |
    \mark\markup\bold"7."
    d4 a8 b c a |
    \mark\markup\bold"6."
    d4 a8 b c a |
    \break
    \time 2/2
    \mark\markup\bold"5."    
    d2^\markup\italic"Slower" e8( cis4.) |
    d1 |
    
    \time 3/4 d8[ c] b a g4 |
    c e, g |
    \time 4/4 a8[ g] fis e d4 b'8 c |
    
    d4^\markup\italic"(last time rall.)" e8[ c] b g a4 |
    \partial 4*3 g2~g8 b8\rest
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d8 d |
  d4 d8 d d4 d8 d |
  fis d d d d4. g8 |
  
  d4 g8[ e] d d fis4 |
  d2~d8 s |
  d d |
  d d d d d4 d8 d |
  
  fis d d d d2 |
  fis4 d8 d c4 g'8 e |
  d4 g8[ e] d d fis4 |
  d2~d8 s |
  
  d d |
  d4 d8 d d4 d8 d |
  fis d d d d2 |
  fis4 d8 d c4 |
  fis4 d4 c4 |
  
  fis4 d8 d c4 g'8 e |
  d4 g8[ e] d d fis4 |
  d2~d8 s |
  
  
  %new stuff 5th verse
  d8 d |
  d4 d8 d d4 d8 d |
  fis d d d d2 |
  
  \repeat unfold 7 {fis4 d8 d c c |}
  
  g'2 g |
  fis1 |
  
  b8[ a] g fis e4 |
  e c e |
  fis8[ e] d8 cis d4 g8 e |
  d4 g8[ e] d d fis4 |
  d2~d8 s |
}
dropLyrics =
{
    \override LyricText.extra-offset = #'(0 . -1.4)
    \override LyricHyphen.extra-offset = #'(0 . -1.4)
    \override LyricExtender.extra-offset = #'(0 . -1.4)
}

altoWords = \lyricmode {
  \dropLyricsIV
  \set stanza = #"1. "
  On the first day of Christ -- mas my true love sent to me
  A par -- tridge in a pear tree. __
  \set stanza = #"2."
  On the sec -- ond day of Christ -- mas my true love sent to me
  Two tur -- tle -- doves and a par -- tridge in a pear tree. __
  
  \set stanza = #"3. "
  On the third day of Christ -- mas my true love sent to me
  \markup\italic"skip to next measure" _ _ _
  Three French hens,
  two tur -- tle -- doves, and a par -- tridge in a pear tree. __
  
  \set stanza = #"5."
  On the fifth \dropLyrics day of Christ -- mas my true love sent to me \raiseLyrics
  
  Twelve drum -- mers drum -- ming,
  Elev’n pip -- ers pip -- ing,
  Ten lords a -- leap -- ing,
  Nine la -- dies dan -- cing,
  Eight maids a -- milk -- ing,
  Sev’n swans a -- swim -- ming,
  Six geese a -- lay -- ing,
  
  Five gold rings,
  Four call -- ing birds,
  Three French hens,
  two tur -- tle -- doves, and a par -- tridge in a pear tree. __
  

}
altoWordsII = \lyricmode {
  \dropLyricsIV
%\markup\italic
  \repeat unfold 46 \skip1
  \set stanza = #"4. "
  On the fourth day of Christ -- mas my true love sent to me
  Four call -- ing birds,
  %Three French hens,
  %two tur -- tle -- doves, and a par -- tridge in a pear tree. __
  
  \repeat unfold 15 \skip1
  \set stanza = #"6–12."
  On the "etc."
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c {
  d8 d |
  b'4 b8 b b4 a8 b |
  a g a a g4. g8 |
  
  g4 g g8 b c4 |
  b2~ b8 s |
  b8 b |
  b b b b b4 a8 b |
  
  a g a a g2 |
  a4 a8 a g4 g8 g |
  g4 g g8 b c4 |
  b2~ b8 s |
  
  b8 b |
  b4 b8 b b4 a8 b |
  a g a a g2 |
  a4 a8 a g4 |
  a4 a4 g4 |
  
  a4 a8 a g4 g8 g |
  g4 g g8 b c4 |
  b2~b8 s |
  
  
  %5th verse
  b8 b |
  b4 b8 b b4 a8 b |
  a g a a g2 |
  
  \repeat unfold 7 {a4 a8 a g fis |}
  
  d'2 cis |
  c!1 |
  
  g4 g8 a b4 |
  g g c |
  c a8 g fis4 g8 g
  g4 g g8 b c4 |
  b2~b8 s |
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d8 d |
  g4 g8 g g4 d8 g |
  d g fis fis g4. e8 |
  
  b4 c d8 d d4 |
  g,2~g8 d'\rest |
  g,8 g |
  g' g g g g4 d8 g |
  
  d g fis fis g2 |
  d4 fis8 fis e4 b8 c |
  b4 c d8 d d4 |
  g,2~g8 d'\rest |
  
  g,8 g |
  g'4 g8 g g4 d8 g |
  d g fis fis g2 |
  d4 fis8 fis e4 |
  d4 fis4 e4 |
  
  d fis8 fis e4 b8 c |
  b4 c d8 d d4 |
  g,2~ g8 d'\rest |
  
  %5th verse
  g,8 g |
  g'4 g8 g g4 d8 g |
  d g fis fis g2 |
  
  \repeat unfold 7 {d4 fis8 fis e d |}
  
  b'2 ais |
  a!1 |
  
  g4 g8 g e4 |
  c c c |
  d d8 d d4 b8 c |
  b4 c d8 d d4 |
  g,2~ g8 d'\rest |
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWords
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

