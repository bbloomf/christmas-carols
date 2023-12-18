\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Carol of the Bells"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Ukrainian Carol)"}}
  poet = \markup\oldStyleNum"Peter J. Wilhousky (1902–1978)"
  composer = \markup\oldStyleNum"Mikola Dmytrovitch Leontovych (1877–1921)"
  tagline = \markup { "from" \italic {HymnsAndCarolsOfChristmas.com}}
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
  first-page-number = #108
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
  \key bes \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-0.5 . 0.5)
}

sopMusic = \relative c'' {
  \tempo 4 = 170
  bes4-> a8 bes g4 |
  bes4-> a8 bes g4 |
  bes4-> a8 bes g4 |
  bes4-> a8 bes g4 | \break
  \repeat volta 2 {
    
    bes4-> a8 bes g4 |
    bes4-> a8 bes g4 |
    bes4-> a8 bes g4 |
    bes4-> a8 bes g4 | \break
    
    bes4-> a8 bes g4 |
    bes4-> a8 bes g4 |
    bes4-> a8 bes g4 |
    bes4-> a8 bes g4 | \break
    
    bes4->\p\< a8 bes g4
    bes4-> a8 bes g4 |
    bes4-> a8 bes g4 |
    bes4-> a8 bes g4 |
    
    d'4->^\mf c8 d bes4 |
    d4 c8 d bes4 |
    d4 c8 d bes4 |
    d4 c8 d bes4 |
    
    % 4th System
    g'4 g8 g f[ ees] |
    d4 d8 d c[ bes] |
    c4 c8 c d[ c] |
    
    g4 g8 g g4 |
    d8 e fis g a bes |
    c d c4 bes |
    
    d,8^\p e fis g a bes |
    c d c4 bes |
    
    %third page
    bes4-> a8 bes g4 |
    bes4-> a8 bes g4 |
    bes4-> a8 bes g4 |
    bes4-> a8 bes g4 |
  }
  \alternative {
    {
      bes4-> a8 bes g4 | 
      bes4-> a8 bes g4 | 
      bes4-> a8 bes g4 | 
      bes4-> a8 bes g4 | \break
    }
    {
      g2. |
      g |
      g |
      g |
      d'4->^\markup\italic"rall." c8-> d-> g,4->~ |
      g2.\fermata \bar "|."
    }
  }
}
sopWordsBelow = \lyricmode {
  Hark! how the bells, sweet sil -- ver bells,
  All seem to say, throw cares a -- way.
  
}
sopWords = \lyricmode {
  \repeat unfold 16 { \skip 1 }
  Christ -- mas is here, bring -- ing good cheer,
  To young and old, meek and the bold,
  
  Ding, dong, ding, dong, that is their song.
  With joy -- ful ring, all ca -- rol -- ing.
  
  \repeat unfold 48 { \skip 1}
  Mer -- ry, mer -- ry, mer -- ry, mer -- ry Christ -- mas!
  Mer -- ry, mer -- ry, mer -- ry, mer -- ry Christ -- mas!
  
  On, on they send, on with -- out end
  Their joy -- ful tone to ev -- ’ry home!
  
  Hark! how the bells, sweet sil -- ver bells
  All seems to say throw cares a -- way.
}

altoMusic = \relative c'' {
  s2.*4 |
  
  g2.-> |
  f-> |
  ees-> |
  d-> |
  
  g2.-> |
  f-> |
  ees-> |
  d-> |
  
  g4-> g8 g g4 |
  g4-> g8 g g4 |
  g4-> g8 g g4 |
  g4-> g8 g g4 |
  
  bes4-> a8 bes g4 |
  bes4-> a8 bes g4 |
  bes4-> a8 bes g4 |
  bes4-> a8 bes g4 |
  
  %third system
  bes4-> bes8 bes bes4 |
  g4-> g8 g g4 |
  g4-> g8 g g4 |
  g4-> g8 g g4 |
  
  d2. |
  e4( fis) g |
  
  d2. |
  e4( fis) g |
  
  %page3
  d2.-> |
  bes-> |
  f'-> |
  ees-> |
  
  d-> |
  d-> |
  d-> |
  d-> |
  
  d-> |
  d-> |
  d-> |
  d-> |
  bes'4-> a8-> bes-> g4->~ |
  g2. \bar "|."
}
altoWords = {
  \lyricmode {
    Ding! Dong! Ding! Dong!
    \dropLyricsV
    Ding! Dong! Ding! Dong!
    
    \set associatedVoice = "sopranos"
    One seems to hear words of good cheer,
    From ev -- ’ry -- where fill -- ing the air.
    
    Oh, how they pound, rais -- ing the sound
    O’er hill and dale, tell -- ing their tale.
    \unset associatedVoice
  }
  \set stanza = \markup\dynamic"f "
  \lyricmode{
    Gai -- ly they ring while peo -- ple sing
    Songs of good cheer, Christ -- mas is \set associatedVoice = "tenors"
    here.
    
    Ding, dong, ding, dong,
    Ding, dong, ding, dong,
    Ding, dong, ding, dong,
    Ding, dong, ding, dong,
    \unset associatedVoice
    Ding! Dong!
    Ding! Dong!
    
    Ding! Dong!
    Ding! Dong!
    
    \raiseLyrics
    Ding! Dong!
    Ding! Dong!
    Ding, dong, ding dong! __
  }
}
tenorMusic = \relative c' {
  s2.*8 |
  
  ees2.-> |
  d-> |
  c-> |
  g-> |
  
  c4-> c8 c c4 |
  d4-> d8 d d4 |
  ees4-> ees8 ees ees4 |
  d4-> d8 d d4 |
  
  d4 d8 d d4 |
  e4 e8 e e4 |
  f4 ees8\noBeam ees d4 |
  g8[ f] ees8 ees d4 |
  
  %third system
  d4 d8 d d4 |
  d4 d8 c d4 |
  d4 c8 d bes4 |
  d4 c8 d bes4 |
  
  bes4 a8 bes g4 |
  bes4 a8 bes g4 |
  bes4 a8 bes g4 |
  bes4 a8 bes g4 |
  
  %page3
  bes2.-> |
  g-> |
  c-> |
  c-> |
  
  bes-> |
  bes-> |
  bes-> |
  bes-> |
  
  
  bes4-> a8 bes g4 |
  bes4-> a8 bes g4 |
  bes4-> a8 bes g4 |
  bes4-> a8 bes g4 |
  
  s2. |
  g2.-> \bar "|."
}
tenorWords = \lyricmode {
  \repeat unfold 76 {\skip 1}
  On, on they send on with -- out end
  Their joy -- ful tone to ev -- ’ry home.
  
  Dong!
}

bassMusic = \relative c {
  s2.*12 |
  
  ees4 ees8 ees8 ees4 |
  g4-> g8 g g4 |
  c4-> c8 c c4 |
  g4-> g8 g g4 |
  
  g4 g8 g g4 |
  g4 g8 g g4 |
  g4 g8 g g4 |
  g4 g8 g g4 |
  
  %third system
  bes4 a8 bes g4 |
  bes4 a8 bes g4 |
  bes4 a8 bes g4 |
  bes4 a8 bes g4 |
  
  d2. |
  d |
  d |
  d |
  
  %page3
  g2.-> |
  ees-> |
  <d a'>-> |
  <c g'>-> |
  
  <g d'>-> |
  q-> |
  q-> |
  q-> |
  
  
  q-> |
  q-> |
  q-> |
  q-> |
  
  d'\rest |
  q->\fermata \bar "|."
}
bassWords = \lyricmode {
  \repeat unfold 52 { \skip 1 }

  \repeat unfold 4 { \skip 1 }
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" } \lyricsto "altos" \altoWords
    \context Lyrics = "altos" \lyricsto "sopranos" \sopWordsBelow
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "basses" \bassWords
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
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup.remove-first = ##t
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

