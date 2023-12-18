\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Deck the Hall"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"16th Century Welsh Tune"
  tagline = \markup \concat{ "from " \italic "Favorite Songs and Hymns for School and Home" \oldStyleNum", 1899, via " \italic"books.google.com"}
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
  first-page-number = #112
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
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-1 . 1)
}

sopMusic = \relative c'' {
  c4. bes8 a4 g |
  f g a f |
  g8\noBeam a\noBeam bes\noBeam g\noBeam a4. g8 |
  f4 e f2 | \break
  
  c'4. bes8 a4 g |
  f g a f |
  g8\noBeam a\noBeam bes\noBeam g\noBeam a4. g8 |
  f4 e f2 | \break
  
  g4. a8 bes4 g |
  a4. bes8 c4 g |
  a8 b c4 d8 e f4 |
  e d c2 | \break
  
  c4. bes8 a4 g |
  f g a f |
  d'8\noBeam d\noBeam d\noBeam d\noBeam c4. bes8 |
  a4 g f2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  a'4. g8 f4 e |
  d e f c |
  e8\noBeam f\noBeam g\noBeam e\noBeam f4. e8 |
  d4 c c2 |
  
  a'4. g8 f4 e |
  d e f f |
  e8\noBeam f\noBeam g\noBeam e\noBeam f4. e8 |
  d4 c c2 |
  
  e4. f8 g4 e |
  f4. g8 f4 c |
  f8 f f4 a8 a a4 |
  g f e2 |
  
  f4. g8 f4 e |
  d e f f |
  bes8 bes bes bes a4. g8 |
  f4 e f2 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsV
  \set stanza = #"1. "
  Deck the hall with boughs of hol -- ly, Fa la la la la, la la la la.
  ’Tis the sea -- son to be jol -- ly, Fa la la la la, la la la la.
  Don we now our gay ap -- par -- el; Fa la la, la la la, la la la.
  Troll the an -- cient Yule -- tide car -- ol, Fa la la la la, la la la la.
}
altoWordsII = \lyricmode {
  \dropLyricsV
  \set stanza = #"2. "
  See the blaz -- ing Yule be -- fore us, Fa la la la la, la la la la.
  Strike the harp and join the cho -- rus, Fa la la la la, la la la la.
  Fol -- low me in mer -- ry mea -- sure, Fa la la, la la la, la la la.
  While I tell of Yule -- tide trea -- sure, Fa la la la la, la la la la.
}
altoWordsIII = \lyricmode {
  \dropLyricsV
  \set stanza = #"3. "
  Fast a -- way the old year pass -- es, Fa la la la la, la la la la.
  Hail the new, ye lads and lass -- es, Fa la la la la, la la la la.
  Sing we joy -- ous all to -- geth -- er, Fa la la, la la la, la la la.
  Heed -- less of the wind and weath -- er, Fa la la la la, la la la la.
  
}
altoWordsIV = \lyricmode {
}

tenorMusic = \relative c' {
  c4. c8 c4 bes |
  a c c a |
  c8\noBeam c\noBeam c\noBeam c\noBeam c4. bes8 |
  a4 bes a2 |
  
  c4. c8 c4 bes |
  a c c a |
  c8\noBeam c\noBeam c\noBeam c\noBeam c4. bes8 |
  a4 bes a2 |
  
  c4. c8 c4 c |
  c4. bes8 a4 c |
  c8 c c4 c8 c c4 |
  c b c2 |
  
  a4. c8 c4 bes |
  a c c a |
  bes8\noBeam bes\noBeam bes\noBeam bes\noBeam c4. bes8 |
  c4 bes a2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  f4. e8 f4 c |
  d c f f |
  c8\noBeam c\noBeam c\noBeam c\noBeam f4. c8 |
  d4 c f2 |
  
  f4. e8 f4 c |
  d c f f |
  c8\noBeam c\noBeam c\noBeam c\noBeam f4. c8 |
  d4 c f2 |
  
  c4. c8 c4 c |
  f4. f8 f4 e |
  f8 g a4 f8 e d4 |
  g g c,2 |
  
  f4. e8 f4 c |
  d c f f |
  bes,8\noBeam bes\noBeam bes\noBeam bes'\noBeam f4. e8 |
  f4 c f2 \bar "|."
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

