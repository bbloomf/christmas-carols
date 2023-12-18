\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"In dulci jubilo"}}
  poet = \markup\oldStyleNum"Heinrich Seuse (1300–1366)"
  meter = \markup\oldStyleNum"Translated by Robert Lucas de Pearsall (1795–1856)"
  composer = \markup\oldStyleNum"14th century German melody"
  arranger = \markup\oldStyleNum"Arranged by Robert Lucas de Pearsall (1795–1856)"
  tagline = \markup { "from" \italic "CantateDomino.org" }
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -5)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #072
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
  \key f \major
  \time 6/4
}

sopMusic = \relative c' {
  \partial 4 f4 |
  f2 f4 a2 bes4 |
  c2( d4 c2) \teeny c4 \normalsize |
  f,2 f4 a2 bes4 |
  c2( d4 c2.) | \break

  c2 d4 c2 bes4 |
  a2. f2 \teeny f4 \normalsize |
  g2 g4 a2 g4 |
  f2( g4 a2) a4 |
  c2 d4 c2 bes4 |
  a2. f2 \bar""\break f4 |

  g2 g4 a2 g4 |
  f2( g4 a2.) |
  d,2 d4 e2 e4 |
  f2.( c'2.) |
  a2 bes4 g2 g4 |
  f2.( f2) \bar""\break f4 |
  
  g2 g4 a2 g4 |
  f2( g4 a2.) |
  d,2 d4 e2 e4 |
  f2.( c'2.) |
  a2 a4 g4( f) e4 |
  \partial 4*5 f2.( f2) \bar "|."

}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  \partial 4 c4 |
  d2 c4 f4.( e8 d4) |
  c2( f4 e2) \teeny f4 \normalsize |
  d2 c4 f4.( e8) d4 |
  c2( f4 e2.) |

  f2 f4 e2 g4 |
  c,2. d2 \teeny f4 \normalsize |
  f2 f4 f2 e4 |
  f2( f4 f2) f4 |
  f2 f4 e2 g4 |
  c,2. f2 f4 |

  f2 f4 f2 e4 |
  f2( f4 f2.) |
  d2 d4 d2 cis4 |
  d2.( e2.) |
  f2 f4 f2 e4 |
  f2.( f2) f4 |

  d2 e4 f2 d4 |
  d2( e4 f2.) |
  f,2 bes4 g2 g4 |
  d'2( f4 f2 e4) |
  f2 e4 d2 c4 |
  \partial 4*5 c2.( c2)

}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  \markup\italic In \markup\italic dul -- \markup\italic ci \markup\italic jú -- \markup\italic bi -- \markup\italic lo __ Let us our hom -- age show: __
  Our heart’s joy re -- clin -- eth "" \markup\italic In \markup\italic præ -- \markup\italic sé -- \markup\italic pi -- \markup\italic o, __
  And like a bright star shin -- eth
  \markup\italic Ma -- \markup\italic tris \markup\italic in \markup\italic gré -- \markup\italic mi -- \markup\italic o __
  \markup\italic Al -- \markup\italic pha \markup\italic es \markup\italic et \markup\italic O! __
  \markup\italic Al -- \markup\italic pha \markup\italic es \markup\italic et \markup\italic O! __

  \markup\italic Ma -- \markup\italic tris \markup\italic in \markup\italic gré -- \markup\italic mi -- \markup\italic o __
  \markup\italic Al -- \markup\italic pha \markup\italic es \markup\italic et \markup\italic O! __
  \markup\italic Al -- \markup\italic pha \markup\italic es \markup\italic et \markup\italic O! __
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  \markup\italic O \markup\italic Je -- \markup\italic su, \markup\italic pár -- \markup\italic vu -- \markup\italic le, __
  I yearn for Thee al -- way; __
  Hear me, I be -- seech Thee,
  \markup\italic O \markup\italic Pu -- \markup\italic er \markup\italic óp -- \markup\italic ti -- \markup\italic me, __
  My pray -- ing let it reach Thee,
  \markup\italic O \markup\italic Prin -- \markup\italic ceps \markup\italic gló -- \markup\italic ri -- \markup\italic æ. __
  \markup\italic Tra -- \markup\italic he \markup\italic me \markup\italic post \markup\italic Te! __
  \markup\italic Tra -- \markup\italic he \markup\italic me \markup\italic post \markup\italic Te! __
  \markup\italic O \markup\italic Prin -- \markup\italic ceps \markup\italic gló -- \markup\italic ri -- \markup\italic æ. __
  \markup\italic Tra -- \markup\italic he \markup\italic me \markup\italic post \markup\italic Te! __
  \markup\italic Tra -- \markup\italic he \markup\italic me \markup\italic post \markup\italic Te! __
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
    \markup\italic Qua -- \markup\italic lis \markup\italic gló -- \markup\italic ri -- \markup\italic a! __
    \markup\italic Qua -- \markup\italic lis \markup\italic gló -- \markup\italic ri -- \markup\italic a! __
  \markup\italic Cæ -- \markup\italic ló -- \markup\italic rum \markup\italic gáu -- \markup\italic di -- \markup\italic a. __
    \markup\italic Qua -- \markup\italic lis \markup\italic gló -- \markup\italic ri -- \markup\italic a! __
    \markup\italic Qua -- \markup\italic lis \markup\italic gló -- \markup\italic ri -- \markup\italic a! __
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  \markup\italic U -- \markup\italic bi \markup\italic sunt \markup\italic gáu -- \markup\italic di -- \markup\italic a __
  "" If they be not there? __
  There are an -- gels sing -- ing ""
  \markup\italic No -- \markup\italic va \markup\italic cán -- \markup\italic ti -- \markup\italic ca; __
  And there the bells are ring -- ing
  \markup\italic In \markup\italic Re -- \markup\italic gis \markup\italic cú -- \markup\italic ri -- \markup\italic a __
  O that we were there! O that we were there!
  \markup\italic In \markup\italic Re -- \markup\italic gis \markup\italic cú -- \markup\italic ri -- \markup\italic a __
  O that we were there! __ O that we were there!
}

tenorMusic = \relative c' {
  \partial 4 a4 |
  bes2 a4 c2 bes4 |
  a2( bes4 g2) \teeny a4 \normalsize |
  bes2 a4 c2 d4 |
  a2( bes4 g2.) |

  c2 bes4 g2 e4 |
  f2. a2 \teeny a4 \normalsize |
  d2 d4 c4.( d8) bes4 |
  a2( bes4 c2) d4 |
  c2 bes4 g2 e4 |
  f2. a2 a4 |

  d2 d4 c4.( d8) bes4 |
  a2( bes4 c2.) |
  a2 a4 g2 g4 |
  a2( bes4 g2.) |
  f2 a4 d2 c4 |
  a2.( a2) a4 |

  bes2 c4 c2 bes4 |
  a2( bes4 c2.) |
  bes2 f4 c'2 c4 |
  a4.( g8 f4 g2.) |
  d'2 c4 bes4.( a8) g4 |
  \partial 4*5 f2.( f2)
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  \partial 4 f4 |
  f2 f4 f2 f4 |
  f2.( f2) \teeny f4 \normalsize |
  f2 f4 f2 f4 |
  f2.( c2.) |

  a2 bes4 c2 c4 |
  f2. d2 \teeny d4 \normalsize |
  bes2 bes4 c2 c4 |
  f2.( f2) d4 |
  a2 bes4 c2 c4 |
  f2. d2 d4 |

  bes2 bes4 c2 c4 |
  f2.( f2.) |
  f2 f4 e2 e4 |
  d2.( c2.) |
  f2 d4 bes2 c4 |
  f2.( f2) d4 |

  g2 c,4 f2 g4 |
  d2( g4 f2.) |
  bes,2 bes4 c2 c4 |
  d2.( c2.) |
  d2 a4 bes2 c4 |
  \partial 4*5 f,2.( f2)

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
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/2)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/2)
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

