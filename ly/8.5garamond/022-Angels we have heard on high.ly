\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Angels We Have Heard on High"}}
  poet = \markup\concat{\italic"Les Anges dans nos Campagnes" \oldStyleNum", 18th Century"}
  meter = \markup\oldStyleNum"Translated by Bishop James Chadwick (1813–1882)"
  composer = \markup\oldStyleNum"18th Century French Carol"
  tagline = \markup \concat{ "from " \italic "Carols Old and Carols New" \oldStyleNum", 1916, via " \italic"HymnsAndCarolsOfChristmas.com"}
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
  first-page-number = #022
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
  \key g \major
  \time 4/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(1 . -0.5)
}

sopMusic = \relative c'' {
  b4 b b d |
  d4. c8 b2 |
  b4 a b d |
  b4. a8 g2 |

  b4 b b d |
  d4. c8 b2 |
  b4 a b d |
  b4. a8 g2 |
  
  \break
  d'2( e8[ d c b] |
  c2 d8[ c b a] |
  b2 c8[ b a g] |
  a4.) d,8 d2 |
  
  g4 a b c |
  b2 a | \break
  d2( e8[ d c b] |
  c2 d8[ c b a] |
  
  b2 c8[ b a g] |
  a4.) d,8 d2 |
  g4 a b c |
  b2( a) |
  g2. b4\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  g4 g g g |
  fis4. fis8 g2 |
  g4 g g g |
  fis4. fis8 g2 |
  
  g4 fis g g |
  fis4. fis8 g2 |
  g4 g g g |
  fis4. fis8 g2 |

  g4( b8[ a] gis4 e~ |
  e a8[ g] fis4 d~ |
  d g8[ fis] e4 c~ |
  c4.) d8 d2 |
  
  d4 d d e |
  d( g) g( fis) |
  g4( b8[ a] gis4 e~ |
  e a8[ g] fis4 d~ |
  
  d g8[ fis] e4 c~ |
  c4.) d8 d2 |
  d4 d d e |
  d( g2 fis4) |
  d2. s4 \bar "|."
}
altoWords = {
  \dropLyricsV
  \lyricmode {
    \set stanza = #"1. "
    An -- gels we have heard on high,
    Sweet -- ly sing -- ing o’er the plains;
    And the moun -- tains in re -- ply
    Ech -- o -- ing their joy -- ous strains.
  }
  \raiseLyrics
  \set stanza = \markup\dynamic"mf  "
  \lyricmode {
    \markup\italic Gló -- \markup\italic ri -- \markup\italic a \markup\italic in \markup\italic ex -- \markup\italic cél -- \markup\italic sis \markup\italic De -- \markup\italic o, __
  }
  \set stanza = \markup\dynamic"f   "
  \lyricmode {
    \markup\italic Gló -- \markup\italic ri -- \markup\italic a \markup\italic in \markup\italic ex -- \markup\italic cél -- \markup\italic sis \markup\italic De -- \markup\italic o!
  }
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  Shep -- herds, why this ju -- bi -- lee?
  Why your joy -- ous songs pro -- long?
  What the glad -- some ti -- dings be
  Which in -- spire your heav’n -- ly song?
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Come to Beth -- le -- hem and see
  Him whose birth the an -- gels sing;
  Come a -- dore on bend -- ed knee
  Christ, the Lord, our new -- born King.
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  See Him in a man -- ger laid,
  Whom the choirs of an -- gels praise;
  Ma -- ry, Jo -- seph, lend your aid,
  While our hearts in love we raise.
}

tenorMusic = \relative c' {
  d4 d d d |
  c d d2 |
  d4 c d b |
  d c b2 |
  
  d4 d e8[ d] c[ b] 
  c4 d d2 |
  d4 e d b |
  d c b2 |
  
  b2( e~ |
  e d~ |
  d c |
  a4) g fis2 |
  
  g4 fis g g |
  g( b) d4.( c8) |
  b2( e~ |
  e d~ |
  
  d c |
  a4) g fis2 |
  g4 fis g g |
  g( b d4. c8) |
  b2. s4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  g4 g g b |
  a d, g2 |
  g4 g g g |
  d d g2 |
  
  g4 b e8[ d] c[ b] |
  a4 d, g2 |
  g4 c b g |
  d d g2 |
  
  g2( e4 gis |
  a2 d,4 fis |
  g2 c,4 e |
  fis) e d( c) |
  
  b a g c |
  d2 d |
  g2( e4 gis |
  a2 d,4 fis |
  
  g2 c,4 e |
  fis) e d( c) |
  b a g c |
  d1 |
  g2. d4\rest \bar "|."
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" } \lyricsto "tenors" \sopWords
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "tenors" \altoWords
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

