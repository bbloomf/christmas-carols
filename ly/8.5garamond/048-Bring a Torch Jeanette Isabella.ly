\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Bring a Torch, Jeanette, Isabella!"}}
  poet = \markup\concat{\italic"Un flambeau, Jeannette, Isabelle" \oldStyleNum", by Émile Blémont (1839–1927)"}
  meter = \markup\oldStyleNum"English by Edward Cuthbert Nunn (1868–1914)"
  composer = \markup\concat{\oldStyleNum"16th Century French Carol"}
  arranger = \markup\oldStyleNum"Arranged by Edward Cuthbert Nunn (1868–1914)"
  tagline = \markup\concat{"from " \italic"The Home and Community Song-Book" \oldStyleNum", 1922"}
}
\paper {
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
  first-page-number = #048
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
  \time 3/8
  %\override DynamicLineSpanner.staff-padding = #0.0
  %\override DynamicLineSpanner.Y-extent = #'(0 . 0)
  \slurDashed
  \tieDashed
}

sopMusic = \relative c'' {
  \tempo "Brightly"
  \override DynamicText.X-offset = #-4
  d4_\f g,8 ||
  \slurDashed g\noBeam( fis\noBeam)  g |
  a\noBeam( b\noBeam) c |
  b4 a8 |
  
  d4 g,8 |
  \slurDashed g\noBeam( fis\noBeam)  g | \break
  a\noBeam( b\noBeam) a |
  g4 b8\rest |
  
  \once\override DynamicText.X-offset = #-5
  d4_\mf d8 |
  d8\noBeam( c\noBeam) b |
  b\noBeam a\noBeam g | 
  \slurDashed g4( fis8) | \break
  
  \once\override DynamicText.X-offset = #-3
  e_\f\noBeam( fis\noBeam) g |
  d4 d8 |
  c'4 c8 |
  b4 a8 \bar "||"
  
  g4_\p b8\rest |
  a4 b8\rest | 
  \slurDashed b8\noBeam( c\noBeam) b | \break
  a4 d8 |
  b4 a8 |
  
  g4_\pp b8\rest |
  a4 b8\rest |
  b\noBeam( c\noBeam) b |
  a4 d8 |
  \tieSolid
  g,4.~ |
  g8 b\rest b\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  g4 d8 |
  \slurDashed e\noBeam~ e\noBeam e |
  e\noBeam~ e\noBeam e |
  d4 d8 |
  
  d4 d8 |
  \slurDashed e\noBeam~ e\noBeam e |
  
  
  fis8\noBeam~ fis\noBeam fis8 |
  g4 s8 |
  
  g4 g8 |
  g\noBeam( fis\noBeam) g |
  d\noBeam d\noBeam cis |
  d4~ d8 |
  
  
  e8\noBeam~ e\noBeam e |
  d4 d8 |
  e4 e8 |
  d4 fis8 |
  
  g4 s8 |
  fis4 s8 |
  g\noBeam~ g\noBeam g |
  
  
  a4 fis8 |
  g4 fis8 |
  
  g4 s8 |
  e4 s8 |
  d\noBeam~ d\noBeam e |
  fis4 fis8 |
  \tieSolid
  g4.~ |
  g8 s4 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Bring a torch, __ _ Jean -- nette, Is -- a -- bel -- la!
  Bring a torch, to the cra -- _ dle, run!
  It is Je -- sus, good folk of the vil -- lage;
  Christ _ is born and Ma -- ry’s call -- ing:
  Ah! ah! beau -- ti -- ful is the Moth -- er;
  Ah! ah! beau -- ti -- ful is her Son! __ _
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Who goes there __ _ a -- knock -- ing so loud -- ly?
  Who goes there _ a -- knock -- ing like that?
  Ope your doors, I have here on a plate
  Some ve -- ry good cakes which I am bring -- ing:
  Toc! toc! quick -- ly your doors now o -- pen;
  Toc! toc! Come let us make good cheer! __ _
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  It is wrong when the Child _ is sleep -- ing,
  It is wrong _ to talk __ _ so loud;
  Si -- lence, all, as you gath -- er a -- round, __ _
  Lest __ _ your noise should wak -- en Je -- sus:
  Hush! hush! see __ _ how fast He slum -- bers!
  Hush! hush! see __ _ how fast He sleeps! __ _
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  \set ignoreMelismata = ##t
  Soft -- ly to __ _ the lit -- _ tle sta -- ble,
  Soft -- ly for __ _ a mo -- _ ment come;
  Look and see __ _ how charm -- ing is Je -- sus,
  How He is white, His cheeks are ros -- y!
  Hush! hush! see how the Child is sleep -- ing;
  Hush! hush! see how He smiles in dreams.
__
  _
}

tenorMusic = \relative c' {
  \override DynamicText.X-offset = #-4
  b4 b8 |
  \slurDashed b8\noBeam( a\noBeam) b |
  a\noBeam( gis\noBeam) a |
  fis4 fis8 |
  
  g4 g8 |
  b8\noBeam( a\noBeam) b |
  
  
  c\noBeam~ c\noBeam c |
  b4 s8 |
  
  d4 d8 |
  d\noBeam~ d\noBeam d\noBeam |
  g,\noBeam a\noBeam a |
  a4~ a8 |
  
  
  c\noBeam~ c\noBeam g |
  g4 g8 |
  g4 g8 |
  fis4 c'8 |
  
  b4 s8 |
  d4 s8 |
  d8\noBeam( e\noBeam) d |
  
  
  d4 d8 |
  d4 c8 |
  
  b4 s8 |
  e,4 s8 |
  g8\noBeam~ g\noBeam g |
  c4 c8 |
  \tieSolid
  b4.~ |
  b8 s4 \bar "|."
  
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  g4 g8 |
  \slurDashed e8\noBeam~ e\noBeam e |
  c\noBeam( b\noBeam) a |
  d4 c8 |
  
  b4 b8 |
  e8\noBeam~ e\noBeam e |
  
  
  d\noBeam~ d\noBeam d |
  g4 d8\rest |
  
  b'4 b8 |
  b\noBeam( a\noBeam) g |
  g\noBeam fis\noBeam e |
  d4~ d8 |
  
  
  c\noBeam~ c\noBeam c |
  b4 b8 |
  a4 a8 |
  d4 d8 |
  
  e4 d8\rest |
  d4 d8\rest |
  g\noBeam~ g\noBeam g |
  
  
  fis4 d8 |
  g4 d8 |
  
  e4 d8\rest |
  c4 d8\rest |
  b\noBeam~ b\noBeam c |
  d4 d8 |
  \tieSolid
  g4.~ |
  g8 d\rest d\rest \bar "|."
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
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" 
      %\override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))
      } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" 
      %\override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))
      } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" 
      %\override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))
      } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" 
      %\override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))
      } \lyricsto "sopranos" \altoWords
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

