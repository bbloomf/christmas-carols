\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"God Rest You Merry, Gentlemen"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"Traditional"
  arranger = \markup\oldStyleNum"Arranged by Sir John Stainer (1840–1901)"
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
  %     (padding . -5)
  %     (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #052
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
  \time 2/2
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \repeat volta 2 {
    \partial 4 e4 |
    e b' b a |
    \slurDashed g( fis) e d |
    \slurSolid e fis g a |
    b2. \bar""\break e,4 |
    
    e b' b a |
    \slurDashed g( fis) e d |
    \slurSolid
    e fis g a |
    b2 b4\rest \bar""\break b |
    
    c a b c |
    \slurDashed d( e) b a |
    \slurSolid
    g e fis g |
    \partial 2 a2 \bar "||" \break
    
    \partial 2 g4( a) |
    b2 c4 b |
    b( a) g fis |
    e2 g8\noBeam fis e4 |
    a2 g4( a) |
    b( c) d e |
    b( a) g fis |
    \partial 4*3 e2. \break
  }
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e4 |
  e g fis dis |
  \slurDashed e( d?) c b |
  \slurSolid e dis e e |
  dis2. e4 |
  
  e g fis dis |
  \slurDashed e( d?) c b |
  \slurSolid
  e dis e e |
  dis2 s4 e |
  
  e d d g |
  \slurDashed f( e) d dis |
  \slurSolid
  e cis d g |
  fis2 |
  
  g4( d) |
  d2 e4 d |
  g( fis) e dis) |
  e2 b8\noBeam b cis4 |
  d2 e4( fis) |
  g2 g4 g |
  g( fis) e dis |
  e2.
}
altoWords = {
  \dropLyricsIX
  \lyricmode {
    \set stanza = #"1. "
    God rest you mer -- ry, \set ignoreMelismata = ##t
    gen -- tle -- men,
    \unset ignoreMelismata
    Let noth -- ing you dis -- may,
    Re -- mem -- ber Christ our Sav -- ior Was born on Christ -- mas Day,
    To save us all from
    \set ignoreMelismata = ##t
    Sa -- tan’s 
    \unset ignoreMelismata
    pow’r When we were gone a -- stray;
  }
  \set stanza = \markup\dynamic"ff  "
  \lyricmode {
    O __ ti -- dings of com -- fort and joy, com -- fort and joy, O __ ti -- dings of com -- fort and joy.
    
    
    \set stanza = #"4. "
    “Fear not then,” said the An -- gel, “Let noth -- ing you af -- fright,
    This day is born a Sav -- ior Of a pure Vir -- gin bright,
    \set ignoreMelismata = ##t
    To free all those who trust in Him From Sa -- tan’s pow’r and might.”
    \unset ignoreMelismata
  }
  \set stanza = \markup\dynamic"ff  "
  \lyricmode {
    O __ ti -- dings of com -- fort and joy, com -- fort and joy, O __ ti -- dings of com -- fort and joy.
  }
}
altoWordsII = {
  \dropLyricsIX
  \set stanza = \markup{\dynamic"  mf " "2. "}
  \lyricmode {
    In Beth -- le -- hem in Jew -- ry, This bless -- ed Babe was born,
    And laid with -- in a man -- ger, Up -- on this bless -- ed Morn;
    The which His Moth -- er Ma -- ry, Did noth -- ing take in scorn.
    
    \repeat unfold 20\skip1
    \set stanza = #"5. "
    The shep -- herds at those ti -- dings Re -- joic -- ed much in mind,
    And left their flocks a -- feed -- ing, In tem -- pest, storm, and wind:
    \set ignoreMelismata = ##t
    And went to Beth -- le -- hem straight way, The Son of God to find.
  }
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  From God our Heav’n -- ly Fa -- ther, A bless -- ed An -- gel came;
  And un -- to cer -- tain Shep -- herds Brought ti -- dings of the same:
  \set ignoreMelismata = ##t
  How that in Beth -- le -- hem was born The Son of God by Name.
  
  \unset ignoreMelismata
  \repeat unfold 20\skip1
  \set stanza = #"6. "
  And when they came to \set ignoreMelismata = ##t
  Beth -- le -- hem
  \unset ignoreMelismata
  Where our dear Sav -- ior lay,
  They found Him in a man -- ger, Where ox -- en feed on hay;
  \set ignoreMelismata = ##t
  His Moth -- er Ma -- ry kneel -- ing down, Un -- to the Lord did pray.
}
altoWordsIV = \lyricmode {
  \repeat unfold 59\skip1
  \dropLyricsIX
  \set stanza = #"7. "
  Now to the Lord sing prais -- es,
  All you with -- in this place,
  And with true love and \set ignoreMelismata = ##t
  bro -- ther -- hood
  \unset ignoreMelismata
  Each oth -- er now em -- brace;
  This ho -- ly tide of Christ -- mas
  All oth -- er doth de -- face.
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
altoWordsVII = \lyricmode {
}

tenorMusic = \relative c {
  e4 |
  g e fis b |
  \slurDashed b( b) g g |
  \slurSolid
  g b b e, |
  fis2. e4 |
  
  g e fis b |
  \slurDashed b( b) g g |
  \slurSolid
  g b b e, |
  fis2 s4 g |
  
  a a g g |
  \slurDashed g( g) g fis |
  \slurSolid
  g g a d |
  d( c) |
  
  b( a) |
  g2 g4 g |
  d'( c) b b |
  g2 g8\noBeam g g4 |
  fis( a) d( c) |
  b( g) d' c |
  d( c) b b |
  g2.
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  e4 |
  e e dis b |
  \slurDashed e( b) c g |
  \slurSolid
  c b e c |
  b2. e4 |
  
  e e dis b |
  \slurDashed e( b) c g |
  \slurSolid
  c b e c |
  b2 d4\rest e |
  
  a fis g e |
  \slurDashed b( c) g b |
  \slurSolid
  e e d b |
  d2 |
  
  e4( fis) |
  g2 c,4 g' |
  g,( a) b b |
  e2 e8\noBeam e e4 |
  d( c) b( a) |
  g( e') b c |
  g( a) b b |
  e2.
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \repeat unfold 2 \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \repeat unfold 2 \altoMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVII
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "altos" \altoWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \repeat unfold 2 \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \repeat unfold 2 \bassMusic >> }
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

