\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Earth Today Rejoices"}}
  poet = \markup\oldStyleNum"John Mason Neale (1818–1866)"
  composer = \markup\concat{\italic "Ave maris stella lucens" ", from " \italic "Piæ Cantiones" \oldStyleNum", 1582"}
  arranger = \markup\oldStyleNum"Arranged by George Ratcliffe Woodward (1848–1934)"
  tagline = \markup\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
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
  first-page-number = #149
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
  \autoBeamOff
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  g4 g a b |
  c( b8[ a]) b2 |
  g4 b a g |
  fis g e2 |
  
  d1 |
  d'2 b |
  c8([ b] c4) a2 |
  c4 e d c |
  b1 |
  
  g4 g a b |
  c4( b8[ a]) b2 |
  g4 b a g |
  fis g e2 |
  
  d1 |
  d'2 b |
  c8([ b] c4) a2 |
  c4 e d c |
  b1 |
  
  c4 b a b |
  c a g2 |
  d4 fis e fis |
  g e d2 |
  
  d'2 b |
  c8[ b] c4 a2 |
  c4 e d c |
  b1\fermata \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  d4 d fis g |
  g( fis) g2 |
  g4 g fis e |
  d d c( b) |
  
  a1 |
  d2 e |
  c4( e) d2 |
  g4 g g4. fis8 |
  g1 |
  
  d4 e fis fis |
  a( e) fis2 |
  e4 g fis e |
  d d d( cis) |
  
  a1 |
  d2 e |
  e8[( d] e4) e2 |
  f4 g f e |
  e1 |
  
  c4 d8[ e] fis4 g |
  g fis d2 |
  d4 d b d |
  d c d2 |
  
  b'2 gis |
  a4 a8[ g] fis2 |
  g4 g fis8[ d] e[ fis] |
  d1 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Earth to -- day re -- joi -- ces,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Death can hurt no more;
  And ce -- les -- tial voi -- ces,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Tell that sin is o’er.
  Da -- vid’s sling de -- stroys the foe:
  Sam -- son lays the tem -- ple low:
  \set associatedVoice = "altos"
  War and strife are done,
  God and man are one.
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  Re -- con -- ci -- li -- a -- tion,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Peace that lasts for aye,
  Glad -- ness and sal -- va -- tion,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Came on Christ -- mas Day.
  Gid -- eon’s Fleece is wet with dew,
  Sol -- o -- mon is crown’d a -- new:
  \set associatedVoice = "altos"
  War and strife are done,
  God and man are one.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Though the cold grows stron -- ger,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Though the world loves night,
  Yet the days grow lon -- ger,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Al -- le -- lu -- ia,
  Christ is born our Light.
  Now the Di -- al’s type is learnt,
  Burns the Bush that is not burnt:
  \set associatedVoice = "altos"
  War and strife are done,
  God and man are one.
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
  b4 b d d |
  e( d) d2 |
  d4 d d b |
  b b g2 |
  
  fis1 |
  fis2 g |
  e4( a8[ g]) fis2 |
  g4 c8[ b] a4 d |
  d1 |
  
  b4 b d d |
  e2 dis |
  b4 d d b |
  b b a2 |
  
  fis1 |
  fis2 g |
  a4.( b8) c2 |
  a4 c a a |
  gis1 |
  
  a4 g d' d |
  e d b2 |
  a4 a g a |
  g g b2 |
  
  fis'2 e |
  e8[ d] e4 d2 |
  c4 c a8[ fis] g[ a] |
  g1 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  g'4 g d g |
  c,( d) g2 |
  b4 g d e |
  b g c2 |
  
  d1 |
  b2 e |
  a, d |
  e4 c d d |
  g1 |
  
  g4 e d b |
  a( c) b2 |
  e4 b d e |
  b g a2 |
  
  d1 |
  b2 e |
  a, a |
  f'4 c d a |
  e'1 |
  
  a,4 b8[ c] d4 g |
  c, d g2 |
  fis4 d e d |
  b c g'2 |
  
  b, e |
  a,4 a d2 |
  e4 c d d |
  g,1 \bar "|."
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
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

