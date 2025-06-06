﻿\version "2.24.0"
\include "util.ly"
\header {
  tagline = ""
}
\paper {
  print-all-headers = ##t
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
  first-page-number = #113
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
  \key aes \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-0.5 . 0.5)
}

sopMusic = \relative c' {
  \partial 4 ees4 |
  aes4 aes8\noBeam bes aes\noBeam g |
  f4 f f |
  bes bes8\noBeam c bes\noBeam aes |
  g4 ees ees |
  
  c' c8\noBeam des c\noBeam bes |
  \slurDashed aes4 f ees8\noBeam( ees) |
  f4 bes g |
  \partial 2 aes2^\markup\italic"Fine" \bar "||" \break
  \partial 4 ees4 |
  aes aes aes |
  
  g2 g4 |
  aes g f |
  ees2 bes'4 |
  c bes aes |
  ees' ees, ees8\noBeam ees |
  f4 bes g |
  \partial 2 aes2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  ees4 |
  c c8\noBeam c c\noBeam c |
  des4 des ees |
  d d8\noBeam d d\noBeam d |
  ees4 ees ees |
  
  e e8\noBeam e e\noBeam e |
  \slurDashed f4 c ees8\noBeam( ees) |
  des4 f ees |
  ees2 \bar "||"
  c4 |
  ees4 ees ees |
  
  ees2 ees4 |
  d4 d d |
  ees2 ees4 |
  ees des c |
  ees ees ees8\noBeam ees |
  des4 f ees |
  \partial 2 ees2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1, 4. "
  \set ignoreMelismata = ##t
  We wish you a Mer -- ry Christ -- mas,
  We wish you a Mer -- ry Christ -- mas,
  We wish you a Mer -- ry Christ -- mas,
  And a hap -- py New Year!
}
altoWordsII = {
  \dropLyricsV
  \set stanza = \markup{\dynamic"mf   " "2. "}
  \lyricmode {
  %\markup\italic
    \set ignoreMelismata = ##t
    Oh, bring us a fig -- gy pud -- ding,
    Oh, bring us a fig -- gy pud -- ding,
    Oh, bring us a fig -- gy pud -- ding,
    and a cup of good cheer.
  }
  \set stanza = \markup\dynamic"mp "
  \lyricmode{
    Good ti -- dings to you wher -- ev -- er you are;
    Good ti -- dings for Christ -- mas and a hap -- py New Year!
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  We won’t go un -- til we get some,
  We won’t go un -- til we get some,
  We won’t go un -- til we get some,
  so __ _ bring it right here.
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
}
tenorMusic = \relative c {
  ees4 |
  ees4 ees8\noBeam ees aes\noBeam aes |
  aes4 aes a |
  bes f8\noBeam aes g\noBeam f |
  ees4 g g |
  
  g g8\noBeam g c\noBeam c |
  \slurDashed c4 aes aes8\noBeam( aes) |
  aes4 des des |
  c2_\markup\italic"Fine" \bar "||"
  aes4 |
  c c c |
  
  bes2 bes4 |
  bes bes aes |
  g2 g4 |
  aes aes aes |
  aes aes aes8\noBeam aes |
  aes4 des des |
  \partial 2 c2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees4 |
  aes, aes8\noBeam aes aes\noBeam aes |
  des4 des c |
  bes bes8\noBeam bes bes\noBeam bes |
  ees4 ees des |
  
  c c8\noBeam c c\noBeam c |
  \slurDashed f4 f c8\noBeam( c) |
  des4 bes ees |
  aes2 \bar "||"
  aes4 |
  aes, aes aes |
  
  bes2 bes4 |
  bes bes bes |
  ees2 ees4 |
  aes, aes aes |
  c c c8\noBeam c |
  des4 bes ees |
  \partial 2 aes2 \bar "|."
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"We Wish You a Merry Christmas"}}
    poet = \markup\oldStyleNum"Traditional"
    composer = \markup\oldStyleNum"English Folk Song"
  }
}




















global = {
  \key g \major
  \time 3/4
}

sopMusic = \relative c'' {
  \once \override Score.RehearsalMark.self-alignment-X = #LEFT
  \mark "Somewhat quickly"
  g2^"I." a4 |
  b2 g4 |
  c2 b4 |
  b( a) g |
  c2 b4 |
  b( a) g \bar "||"
  
  b2^"II." c4 |
  d2 b4 |
  e2 d4 |
  d( c) b |
  e2 d4 |
  d( c) b \bar "||"
  
  g2.^"III." |
  g2. |
  g2. |
  g2. |
  g2. |
  g2. \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  
}
altoWords = \lyricmode {
  Oh, how love -- ly is the eve -- ning, is the eve -- ning,
  When the Christ -- mas bells are ring -- ing, sweet -- ly ring -- ing!
  Ding, dong, ding, dong, ding, dong.
}
altoWordsII = \lyricmode {
}
altoWordsIII = \lyricmode {
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
   
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Christmas Bells"}}
    subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Lovely Evening)"}}
  }
}

