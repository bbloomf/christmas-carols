\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"O Come, O Come, Emmanuel"}}
  poet = \markup\oldStyleNum"Translated by John Mason Neale (1818–1866)"
  composer = \markup\oldStyleNum"15th Century French"
  tagline = \markup ""
}
\paper {
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -10)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #002
  print-first-page-number = ##t
  headerLine = \markup{\override #'(font-name . "Garamond Premier Pro") \smallCapsOldStyle advent}
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \fill-line{"" \oldStylePageNum"" }
        \fill-line{\headerLine }
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
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
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \repeat volta 2{
    \partial 4 e4 |
    g b b b |
    a( c b) a |
    g2. a4 |
    b g e g |

    a( fis e) d |
    e2. \bar""\break a4 |
    a e e fis |
    g2( fis4) e | 

    d2. g4 |
    a b b b |
    a( c b) a |
    \partial 2. g2. \bar "||" \break

    \partial 4 d'4 |
    d2. b4 |
    b2. b4 |
    a( c b) a |
    g2. a4 |
    b g e g |
    a( fis e) d |
    \partial 2. <<e2. {s2^\> s8. s16\!}>> \break
  }



  \partial 4 e4 |
  g b b b |
  a( c b) a |
  g2. a4 |
  b g e g |

  a( fis e) d |
  e2. \bar""\break a4 |
  a e e fis |
  g2( fis4) e |

  d2. g4 |
  a b b b |
  a( c b) a |
  \partial 2. g2. \bar "||" \break
}
sopWords = \lyricmode {

}

altoMusic = \relative c' {
  b4 |
  e b d g |
  g2. fis4 |
  g2. d4 |
  d d c b |

  c2(  b4) b |
  b2. e8[ d] |
  c4 c e dis |
  e2( d4) cis |

  d2. b4 |
  d d d g |
  g2. fis4 |
  g2. |

  g4 |
  fis2. g4 |
  fis2. g4 |
  g2. fis4 |
  g2. fis4 |
  g d e e |
  e( c b) b |
  b2.




  b4 |
  e b d g |
  g2. fis4 |
  g2. d4 |
  d d c b |

  c2(  b4) b |
  b2. e8[ d] |
  c4 c e dis |
  e2( d4) cis |

  d2. b4 |
  d d d g |
  g2. fis4 |
  g2. |
}
altoWords = \lyricmode {
  \dropLyricsVII
  \set stanza = #"1. "
  O come, O come, Em -- man -- u -- el,  And ran -- som cap -- tive Is -- ra -- el,
  That mourns in lone -- ly ex -- ile here  Un -- til the Son of God __ ap -- pear.
  Re -- joice! Re -- joice! Em -- man -- u -- el Shall come to thee, O Is -- ra -- el.


  \set stanza = #"5. "
  O come, Thou Key of Da -- vid, come, And o -- pen wide our heav’n -- ly home;
  Make safe the way that leads __ on high, And close the path to mis -- er -- y.
}
altoWordsII = \lyricmode {
  \dropLyricsVII
%\markup\italic
  \set stanza = #"2. "
  O come, Thou Wis -- dom from __ on high,
  Who ord -- ’rest all things might -- i -- ly;
  To us the path of know -- ledge show,
  And teach us in her ways __ to go.


  \repeat unfold 16 \skip1
  \set stanza = #"6. "
  O come, Thou Day -- Spring, come __ and cheer Our spi -- rits by Thine ad -- vent here;
  Dis -- perse the gloom -- y clouds __ of night, And death’s dark shad -- ows put __ to flight.

}
altoWordsIII = \lyricmode {
  \dropLyricsVII
  \set stanza = #"3. "
  O come, O come, Thou Lord __ of might, Who to Thy tribes, on Si -- nai’s height,
  In an -- cient times didst give __ the Law, In cloud, and maj -- es -- ty __ and awe.

  \repeat unfold 16 \skip1
  \set stanza = #"7. "
  O come, De -- sire of na -- tions, bind
  In one the hearts of all __ man -- kind;
  Bid Thou our sad di -- vis -- ions cease,
  And be Thy -- self our King __ of Peace.
}
altoWordsIV = \lyricmode {
  \dropLyricsVII
  \set stanza = #"4. "
  O come, Thou Rod of Jes -- se, free Thine own from Sa -- tan’s ty -- ran -- ny;
  From depths of hell Thy peo -- ple save, And give them vic -- t’ry o’er __ the grave.
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
  %\set stanza = #"6. "
}
tenorMusic = \relative c' {
  g4 |
  b fis g d' |
  e2( d4) c |
  b2. fis4 |
  g g g g |

  e( a g) fis |
  g2. a4 |
  a a b a |
  b( a a) g |

  fis2. g4 |
  fis fis g d' |
  e2( d4) c |
  b2. |

  b4 |
  a2. e'4 |
  d2. d4 |
  e2( d4) c |
  b2. d4 |
  d d c b |
  a2( g4) fis |
  <<g2. {s2_\> s8. s16\!}>>




  g4 |
  b fis g d' |
  e2( d4) c |
  b2. fis4 |
  g g g g |

  e( a g) fis |
  g2. a4 |
  a a b a |
  b( a a) g |

  fis2. g4 |
  fis fis g d' |
  e2( d4) c |
  b2. |
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  e4 |
  e d b g |
  c( a b) d |
  e2. d4 |
  g, b c e |

  a,2( b4) b |
  e2. c8[ b] |
  a4 a' g fis |
  e( cis d) a |

  d2. e4 |
  d b g b |
  c( a b) d |
  g,2. |

  g'4 |
  d2. e4 |
  b2. g4 |
  c( a b) d |
  e2. d4 |
  g b, c e |
  c( a b) b |
  e2.




  e4 |
  e d b g |
  c( a b) d |
  e2. d4 |
  g, b c e |

  a,2( b4) b |
  e2. c8[ b] |
  a4 a' g fis |
  e( cis d) a |

  d2. e4 |
  d b g b |
  c( a b) d |
  g,2. |
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
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "sopranos" \altoWords
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
