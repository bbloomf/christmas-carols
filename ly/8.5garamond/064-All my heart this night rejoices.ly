\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"All my heart this night rejoices"}}
  poet = \markup\concat{\italic"Fröhlich soll mein Herze springen" ", by Paul Gerhardt, 1653"}
  meter = \markup\oldStyleNum"Translated by Catherine Winkworth, 1858"
  composer = \markup\oldStyleNum"Johann Georg Ebeling (1637–1676)"
  tagline = \markup { "from" \italic "CantateDomino.org"}
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
  first-page-number = #064
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
  \time 4/4
  \autoBeamOff
}

sopMusic = \relative c' {
  f2 g |
  a2. g4 |
  a c d2 |
  c1 | \break
  c4 d bes2 |
  bes4 c a2 |
  
  a4 c bes a |
  g2 f | \break
  c' bes |
  a2. g4 |
  f e d2 |
  c1 | \break
  
  c'4 d bes2 |
  bes4 c a2 |
  a4 g bes a |
  g2 f2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  c2 d4( e) |
  f2. e4 |
  f e g2 |
  e1 |
  f4 f d2 |
  d4 e c2 |
  
  c4 c f f |
  f( e) c2 |
  f f4( e) |
  f2. c4 |
  c c c( b) |
  c1 |
  
  e4 f d2 |
  d4 e c2 |
  d4 d8[ c] bes4 c |
  d( c) a2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  All my heart this night re -- joi -- ces,
  As I hear, Far and near,
  Sweet -- est an -- gel voi -- ces;
  “Christ is born,” their choirs are sing -- ing,
  \set associatedVoice = "sopranos"
  Till the air, Ev -- ’ry -- where,
  \unset associatedVoice
  Now with joy is ring -- ing.
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  Hark! a voice from yon -- der man -- ger,
  Soft and sweet, Doth en -- treat,
  “Flee from woe and dan -- ger;
  Breth -- ren come; from all that grieves you.
  \set associatedVoice = "sopranos"
  You are freed; All you need
  \unset associatedVoice
  I will sure -- ly give you.”
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Come then let us hast -- en yon -- der;
  Here let all, Great and small,
  Kneel in awe and won -- der.
  Love Him who with love is yearn -- ing;
  \set associatedVoice = "sopranos"
  Hail the star that from far
  \unset associatedVoice
  bright with hope is burn -- ing.
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  Thee, dear Lord, with thee I cher -- ish;
  Live to thee, and with thee, Dy -- ing shall not per -- ish;
  But shall swell with thee for ev -- er,
  \set associatedVoice = "sopranos"
  Far on high, in the joy
  \unset associatedVoice
  that can al -- ter nev -- er.
}

tenorMusic = \relative c' {
  a2 bes |
  c2. c4 |
  c c c( b) |
  c1 |
  a4 a bes2 |
  g4 g a2 |
  
  f4 g8[ a] bes4 c |
  c2 a |
  a bes |
  c2. e,4 |
  f g a( g) |
  e1 |
  g4 a a( g) |
  bes a8[ g] f2 |
  f4 g d8[ e] f4 |
  f( e) c2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c' {
  a2 g |
  f2. c4 |
  f a g2 |
  c,1 |
  f4 d g2 |
  g4 c, f2 |
  
  f4 e d f |
  c2 f |
  f, g |
  a2. c4 |
  d e f( g) |
  c,1 |
  
  c4 f, g2 |
  g'4 c, f( e) |
  d bes8[ a] g4 a |
  bes( c) f,2 \bar "|."
}
bassWords = \lyricmode {

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
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "basses" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \altoWords
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

