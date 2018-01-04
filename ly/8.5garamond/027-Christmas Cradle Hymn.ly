\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Christmas Cradle Hymn"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Lulajże, Jezuniu)"}}
  poet = \markup\oldStyleNum"Traditional Polish Carol"
  meter = \markup\oldStyleNum"Adapted to English by Mary Strawn Vernon"
  composer = \markup\oldStyleNum"Traditional Polish Carol"
  % arranger = \markup\oldStyleNum"Arranged by Edith M. G. Reed (1885–1933)"
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
  first-page-number = #022
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
global = {
  \key ees \major
  \time 3/4
  \autoBeamOff
}

sopMusic = \relative c' {
  g'4 g8[ f] g4 |
  aes aes g |
  f f g8[ aes] |
  c4( bes2) |
  g4 g8[ f] g4 |
  
  aes aes g |
  f bes4. aes8 |
  aes4( g2) |
  \repeat volta 2 {
    bes4 bes ees8[ d] |
    c4 c8[ bes] c[ bes] |

    aes4 aes c |
    c( bes2) |
    g4 g8[ f] g4 |
    aes aes g |
    f bes4. aes8 |
    aes4( g2)
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  ees4 ees ees |
  ees ees ees |
  d d d |
  ees~ ees2 |

  ees4 ees ees |
  ees ees ees |
  d d4. f8 |
  f4( ees2) |

  \repeat volta 2 {
    ees4 ees bes' |
    aes ees ees |
    d8[ ees] f[ ees] aes4 |
    aes( g2) |

    ees4 ees ees |
    ees ees ees |
    d d4. f8 |
    f4( ees2) |
  }
}
altoWords = \lyricmode {
  \set stanza = "1. "

  Hush Thee, my lit -- tle One,
  Moth -- er is nigh;
  At Thy low man -- ger bed
  Watch -- ing close by.
  O -- ver the mead -- ows dim
\set ignoreMelismata = ##t
   \once \override LyricText.self-alignment-X = #LEFT Night
   \once \override LyricText.self-alignment-X = #LEFT winds 
   \once \override LyricText.self-alignment-X = #LEFT are blow -- ing,
\unset ignoreMelismata
  Here in the qui -- et byre,
\set ignoreMelismata = ##t
  Meek cat -- tle low -- ing.
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = "2. "

  Look lit -- tle wake -- ful One
  Out at the sky,
  Where stars like sil -- ver lamps
  Swing far on high.
  Dost know, Child, how three kings
  \once \override LyricText.self-alignment-X = #LEFT Rid --
  \once \override LyricText.self-alignment-X = #LEFT ing
  \once \override LyricText.self-alignment-X = #LEFT from far
  Brought to Thy crib rich gifts,
  Led by Thy star.
}
altoWordsIII = \lyricmode {
  \set stanza = "3. "

  Dost know how, on the night
  When Thou wert born,
  An -- gels sang songs of joy
  From dark till dawn.
  But, now, my Ba -- by, rest,
  \once \override LyricText.self-alignment-X = #LEFT While
  \once \override LyricText.self-alignment-X = #LEFT I
  \once \override LyricText.self-alignment-X = #LEFT am nigh,
  Sleep, drow -- sy lit -- tle Boy,
  Hush, hush -- a -- by.
}
altoWordsIV = \lyricmode {
  \set stanza = "4. "
  \set ignoreMelismata = ##t
}
altoWordsV = \lyricmode {
  \set stanza = "5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = "6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  bes4 bes b |
  c c bes?8[ ees] |
  d[ c] bes[ aes] g[ f] |
  aes4( g2) |

  bes4 bes ees8[ des] |
  c4 c bes8[ ees] |
  d?[ c] bes[ c] d4 |
  c( bes2) |

  \repeat volta 2 {
    g4 g8[ f] g4 |
    aes aes8[ g] aes[ g] |
    f[ ees] d[ c] d[ ees] |
    aes4( g2) |

    bes4 bes ees8[ des] |
    c4 c bes |
    d?8[ c] bes[ c] d4 |
    c( bes2) |
  }
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees4 ees ees |
  aes aes ees |
  bes bes bes |
  ees~ ees2 |

  ees4 ees ees |
  aes aes ees |
  bes bes4. bes8 |
  ees4~ ees2 |

  \repeat volta 2 {
    ees4 ees ees |
    ees ees ees |
    bes bes aes |
    ees'~ ees2 |
    
    ees4 ees ees |
    aes aes ees |
    bes bes4. bes8 |
    ees4~ ees2 |
  }
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWords
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
}

