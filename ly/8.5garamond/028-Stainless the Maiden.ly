\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Stainless the Maiden"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Serdeczna Matko)"}}
  poet = \markup\oldStyleNum"Traditional Polish Hymn"
  %meter = \markup\oldStyleNum"Translated by ??? (1885–1933)"
  composer = \markup\oldStyleNum"Traditional Polish Hymn"
  %arranger = \markup\oldStyleNum"Arranged by ??? (1885–1933)"
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
  first-page-number = #023
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
  \key d \major
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  a'2 a4 g |
  fis2 fis |
  fis4 fis e fis |
  a2 g |
  g g4 fis |
  e2 e |
  b'4 b a g |
  g2 fis |

  \repeat volta 2 {
    a2 a4 a |
    b2 b |
    cis4 cis b cis |
    d2 a |
    d cis4 b |
    b2 a |
    b4 a g cis, |
    e2 d
  }
}
sopWords = \lyricmode {
}

altoMusic = \relative c' {
  fis2 fis4 e |
  d2 d |
  d4 d cis d |
  fis2 e |
  e e4 d |
  cis2 cis |
  g'4 g fis e |
  e2 d |

  \repeat volta 2 {
    fis fis4 fis |
    g2 g |
    g4 g g g |
    fis2 fis |
    b a4 g |
    g2 fis |
    g4 fis e cis |
    cis2 d
  }
}
altoWords = \lyricmode {
  \set stanza = #"1. "

  Stain -- less the Maid -- en
  Whom He chose for moth -- er;
  Nine months she wait -- ed,
  Bear -- ing Christ, our broth -- er;
  Think of her glad -- ness
  When at last she saw Him:
  God in a man -- ger,
  Beth -- le -- hem a heav -- en!
}
altoWordsII = \lyricmode {
  \set stanza = #"2. "

  Lan -- tern in dark -- ness,
  When the sick are sigh -- ing,
  Thresh -- old of bright -- ness,
  Com -- fort of the dy -- ing,
  High she is hold -- ing
  For a world a -- dor -- ing,
  Hope of the na -- tions,
  Je -- sus Christ, our broth -- er.
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
tenorMusic = \relative c' {
  a2 a4 a |
  a2 a |
  a4 a a a |
  b2 b |
  a a4 a |
  a2 a |
  a4 a a a |
  a2 a |

  \repeat volta 2 {
    d2 d4 d |
    d2 d |
    e4 e e e |
    d2 d |
    d2 d4 d |
    d2 a |
    b4 b b g |
    g2 fis
  }
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  d2 d4 e |
  fis2 d |
  fis4 fis g fis |
  dis2 e |
  cis cis4 d |
  e2 a, |
  cis4 cis d e |
  a,2 d |

  \repeat volta 2 {
    d' d4 cis |
    b2 b |
    a4 a a ais |
    b2 fis |
    g g4 g |
    d2 d |
    dis4 dis e e |
    a,2 d
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \altoWords
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

