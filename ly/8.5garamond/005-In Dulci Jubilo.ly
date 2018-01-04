﻿\version "2.14.2"
\include "util.ly"
\paper {
  print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  score-markup-spacing = 
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  markup-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 40))
  
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #005
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle"christmas"}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
\header {
  tagline = \markup { "from" \italic "ChristmasCarolMusic.org" }
}
global = {
  \key f \major
  \time 6/8
}

sopMusic = \relative c' {
  \partial 8 f8 |
  f4 f8 a4 bes8 |
  c4( d8 c4) c8 |
  f,4 f8 a4 bes8 |
  c4( d8 c4) b8\rest |
  c4 d8 c4 bes8 |
  
  a4( g8) f4 \teeny f8 \normalsize |
  g4 g8 a4 g8 |
  f4( g8 a4) a8 |
  c4 d8 c4 bes8 |
  a4( g8) f4 \bar"" \break f8 |
  
  g4 g8 a4 g8 |
  f4( g8 a4) b8\rest |
  d,4 d8 e4 e8 |
  f4.( c'4) b8\rest |
  a4 a8 g4 g8 |
  f4. b4\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  c8 |
  d4 c8 f[ e] d |
  c4( f8 e4) f8 |
  d4 c8 f[ e] d |
  c4( f8 e4) s8 |
  f4 f8 e4 g8 |
  
  f4( e8) f4 \teeny f8 \normalsize |
  f4 f8 e4 e8 |
  f4.~ f4 f8 |
  f4 f8 e4 g8 |
  f4( e8) f4 d8 |
  
  f4 f8 e4 e8 |
  f4( e8 f4) s8 |
  a,4 a8 d4 cis8 |
  d4.( e4) s8 |
  c4 f8 f4 e8 |
  f4. s4 \bar "|."

}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  \markup\italic In \markup\italic dul -- \markup\italic ci \markup\italic jú -- \markup\italic bi -- \markup\italic lo __ Now sing with hearts a -- glow!
__
  Our de -- light and plea -- sure Lies \markup\italic in \markup\italic præ -- \markup\italic sé -- \markup\italic pi -- \markup\italic o, __
  Like sun -- shine is our trea -- sure
  \markup\italic Ma -- \markup\italic tris \markup\italic in \markup\italic gré -- \markup\italic mi -- \markup\italic o __
  \markup\italic Al -- \markup\italic pha \markup\italic es \markup\italic et \markup\italic O! __
  \markup\italic Al -- \markup\italic pha \markup\italic es \markup\italic et \markup\italic O!
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  \markup\italic O \markup\italic Je -- \markup\italic su, \markup\italic pár -- \markup\italic vu -- \markup\italic le, __
  For thee I long al -- way; __
  Com -- fort my heart’s blind -- ness
  \markup\italic O \markup\italic Pu -- \markup\italic er \markup\italic óp -- \markup\italic ti -- \markup\italic me, __
  With all Thy lov -- ing kind -- ness,
  \markup\italic O \markup\italic Prin -- \markup\italic ceps \markup\italic gló -- \markup\italic ri -- \markup\italic æ. __
  \markup\italic Tra -- \markup\italic he \markup\italic me \markup\italic post \markup\italic Te! __
  \markup\italic Tra -- \markup\italic he \markup\italic me \markup\italic post \markup\italic Te!
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  \markup\italic O \markup\italic Pa -- \markup\italic tris \markup\italic cá -- \markup\italic ri -- \markup\italic tas! __
  \markup\italic O \markup\italic na -- \markup\italic ti \markup\italic lé -- \markup\italic ni -- \markup\italic tas! __
  Deep -- ly were we stain -- ed
  \markup\italic Per \markup\italic nos -- \markup\italic tra \markup\italic crí -- \markup\italic mi -- \markup\italic na; __
  But Thou for us hast gain -- ed
  \markup\italic Cæ -- \markup\italic ló -- \markup\italic rum \markup\italic gáu -- \markup\italic di -- \markup\italic a. __
  O that we were there! O that we were there! 
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  \markup\italic U -- \markup\italic bi \markup\italic sunt \markup\italic gáu -- \markup\italic di -- \markup\italic a __
  In an -- y place  but there?
  There are an -- gels sing -- ing ""
  \markup\italic No -- \markup\italic va \markup\italic cán -- \markup\italic ti -- \markup\italic ca __
  And there the bells are ring -- ing
  \markup\italic In \markup\italic Re -- \markup\italic gis \markup\italic cú -- \markup\italic ri -- \markup\italic a __
  O that we were there! O that we were there!
}

tenorMusic = \relative c' {
  a8 |
  bes4 a8 c4 bes8 |
  a4( bes8 g4) a8 |
  bes4 a8 c4 bes8 |
  a4( bes8 g4) s8 |
  c4 bes8 g4 c8 |
  
  c4. a4 \teeny a8 \normalsize |
  d4 d8 c4 bes8 |
  a4( bes8 c4) c8 |
  c4 bes8 g4 c8 |
  c4. a4 a8 |
  
  d4 d8 c[ d] bes |
  a4( c8)~ c4 s8 |
  a4 a8 g4 a8 |
  a4( bes8 g4) s8 |
  f4 a8 d4 c8 |
  a4. s4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f8 |
  f4 f8 f4 f8 |
  f4.~ f4 f8 |
  f4 f8 f4 f8 |
  f4.( c4) d8\rest |
  a4 bes8 c4 e8 |
  
  f4( c8) d4 \teeny d8 \normalsize |
  bes4 bes8 c4 c8 |
  f4.~ f4 f8 |
  a,4 bes8 c4 e8 |
  f4( c8) d4 d8 |
  
  bes4 bes8 c4 c8 |
  d4( c8 f4) d8\rest |
  f4 f8 e4 a,8 |
  d4.( c4) d8\rest |
  f4 d8 bes4 c8 |
  f4. d4\rest \bar "|."

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
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWords
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
    \Lyrics
    \override LyricText #'font-size = #1.3
  }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
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
  \header {
    title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"In dulci jubilo"}}
    poet = \markup\oldStyleNum"Heinrich Seuse (1300–1366)"
    composer = \markup\oldStyleNum"14th century German melody"
    
    tagline = \markup { "from" \italic "ChristmasCarolMusic.org" }
  }
}

