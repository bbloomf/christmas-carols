\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Blessed be that Maid Marie"}}
  poet = \markup\oldStyleNum"15th Century Middle English Carol, modernized"
  composer = \markup\concat {"Melody from William Ballet’s " \italic"Lute Book" \oldStyleNum", c. 1600"}
  arranger = \markup\oldStyleNum"Arranged by Charles Wood (1866–1926)"
  tagline = \markup\concat { "from " \italic "The Cowley Carol Book" \oldStyleNum", 1919"}
}
\paper {
  %print-all-headers = ##t
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
  first-page-number = #099
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
}

sopMusic = \relative c' {
  b'4 e e d |
  e d8[ c] b4.( c8) |
  d[ e] d[ c] b4 a8[ g] |
  
  fis8[ e] fis[ g] e2 |
  g8[ a] b[ c] b4 a8[ g] |
  fis[ e] fis[ g] e2 |
  
  g8[ a] b[ c] b4 a8[ g] |
  fis[ e] fis[ g] e2 \bar "||" \break
  g4\segno g d d |
  
  e8[ fis] g[ a] b4.( c8) |
  d[ e] d[ c] b4 a8[ g] |
  fis[ e] fis[ g] e2\fermata \bar ":|." \break
  \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
  \mark "Fine."
  
  
  b'4 e e d |
  e d8[ c] b4.( c8) |
  d[ e] d[ c] b4 a8[ g] |
  
  fis8[ e] fis[ g] e2 |
  g8[ a] b[ c] b4 a8[ g] |
  fis[ e] fis[ g] e2 |
  
  g8[ a] b[ c] b4 a8[ g] |
  fis[ e] fis[ g] e2 \bar "||"
  \once \override Score.RehearsalMark.break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark.self-alignment-X = #RIGHT
  \mark "D.S. al Fine."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e4 e e8[ fis] g[ a] |
  b4 a g2 |
  g4 a g e |
  
  c c b( c) |
  b e d e |
  d d d( c) |
  
  d d8[ e] d4 e |
  d d e2 \bar "||"
  d4 c a d |
  
  d c8[ e] d2 |
  g4 a g e |
  d d d8([ c] b4) |
  
  
  
  
  e4 e e8[ fis] g[ a] |
  b4 a g2 |
  g4 a g e |
  
  c c b( c) |
  b e d e |
  d d d( c) |
  
  d d8[ e] d4 e |
  d d e2 \bar "||"
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  \set associatedVoice = "basses"
  Bless -- ed
  \set associatedVoice = "altos"
  be __ that 
  \unset associatedVoice
  Maid Ma -- rie; __
  Born He was of her \set associatedVoice = "altos" bo -- \unset associatedVoice dy;
  
  Ve -- ry __ God ere time be -- gan,
  \set associatedVoice = "tenors"
  Born in __
  \unset associatedVoice
  time the Son of Man.
  \markup\italic E -- \markup\italic ya! \markup\italic Je -- \markup\italic sus \markup\italic hó -- \markup\italic di -- \markup\italic e __
  \markup\italic Na -- \markup\italic tus \markup\italic est \markup\italic de \markup\italic Vír --
  \set associatedVoice = "tenors"
  \markup\italic gi -- \markup\italic ne. __
  
  
  
  
  \set stanza = #"4. "
  \set associatedVoice = "basses"
  Fare three
  \set associatedVoice = "altos"
  Kings from
  \unset associatedVoice
  far -- off land, __
  In -- cense, gold and myrrh \set associatedVoice = "altos" in \unset associatedVoice hand;
  In Beth -- lem the Babe they see,
  \set associatedVoice = "tenors"
  \markup\italic Stel -- \markup\italic la
  \unset associatedVoice
  \markup\italic duc -- \markup\italic ti \markup\italic lú -- \markup\italic mi -- \markup\italic ne.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  \set associatedVoice = "basses"
  In a
  \set associatedVoice = "altos"
  man -- ger
  \unset associatedVoice
  of an ass __
  Je -- su lay and lull -- \set associatedVoice = "altos" ed \unset associatedVoice was;
  
  Born to __ die up -- on the Tree
  \set associatedVoice = "tenors"
  \markup\italic Pro \markup\italic pec --
  \unset associatedVoice
  \markup\italic cán -- \markup\italic te \markup\italic hó -- \markup\italic mi -- \markup\italic ne.
  
  
  \repeat unfold 14 \skip1
  
  \set stanza = #"5. "
  \set associatedVoice = "basses"
  Make we
  \set associatedVoice = "altos"
  mer -- ry
  \unset associatedVoice
  on this fest, __
  \markup\italic In \markup\italic quo \markup\italic Chris -- \markup\italic tus \markup\italic na -- \set associatedVoice = "altos" \markup\italic tus \unset associatedVoice \markup\concat{\italic "est" ";"}
  On this Child I pray you call,
  \set associatedVoice = "tenors"
  To as --
  \unset associatedVoice
  soil and save us all.
}
altoWordsIII = \lyricmode {
  \dropLyricsIX
  \set stanza = #"3. "
  \set associatedVoice = "basses"
  Sweet and
  \set associatedVoice = "altos"
  bliss -- ful
  \unset associatedVoice
  was the song __
  Chant -- ed of the An -- \set associatedVoice = "altos" gel \unset associatedVoice throng,
  
  “Peace on earth,” Al -- le -- lu -- ia.
  \set associatedVoice = "tenors"
  \markup\italic In \markup\italic ex --
  \unset associatedVoice
  \markup\italic cél -- \markup\italic sis \markup\italic gló -- \markup\italic ri -- \markup\italic a.
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  g4 g8[ a] b[ c] d4 |
  g, a d4.( c8) |
  b4 a d b |
  
  a a e2 |
  e8[ fis] g[ a] b4 b |
  b b g2 |
  
  g4 fis8[ g] g[ a] b4 |
  a d, g2 \bar "||"
  g4 e fis g |
  
  g c b4.( a8) |
  b4 a d b |
  a d, e8[( fis] gis4) |
  
  
  
  
  g4 g8[ a] b[ c] d4 |
  g, a d4.( c8) |
  b4 a d b |
  
  a a e2 |
  e8[ fis] g[ a] b4 b |
  b b g2 |
  
  g4 fis8[ g] g[ a] b4 |
  a d, g2 \bar "||"
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  e4 e8[ fis] g[ a] b4 |
  e, fis g2 |
  g4 fis g g |
  
  a a, g( a) |
  e' e8[ c] g'4 g |
  b b, c2 |
  
  b4 d8[ c] g'4 g |
  d b c8([ d] e4) \bar "||"
  b4 c d b |
  
  c8[ d] e[ c] g'2 |
  g4 fis g g, |
  d' b a4( e')\fermata |
  
  
  
  
  e4 e8[ fis] g[ a] b4 |
  e, fis g2 |
  g4 fis g g |
  
  a a, g( a) |
  e' e8[ c] g'4 g |
  b b, c2 |
  
  b4 d8[ c] g'4 g |
  d b c8([ d] e4) \bar "||"
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
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5))} \lyricsto "sopranos" \altoWords
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

