\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Ave Jesu Deus"}}
  poet = \markup\oldStyleNum"Anonymous"
  composer = \markup\oldStyleNum"Sir John Stainer (1840–1901)"
  tagline = \markup { "from" \italic {Christmas Carols, New and Old}}
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
  first-page-number = #121
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
  \key ees \major
  \time 2/4
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(1 . 1)
  \override DynamicText.X-offset = #'-4
}

sopMusic = \relative c' {
  ees4 f |
  bes aes |
  g f |
  f ees \bar "||"
  f4. g8 |
  aes4 c |
  
  bes aes |
  aes g \bar "||"
  bes c |
  d4. d8 |
  d4 c |
  bes a \bar "||"
  ees'_\p d |
  
  c bes |
  bes a |
  g g \bar "||" \break
  g4. f8 |
  ees4 ees |
  aes4. g8 |
  g4 f \bar "||"
  
  bes4^\markup\italic"cresc." bes |
  g ees |
  c'4. bes8 |
  bes4( aes) \bar "||" \break
  c4 c |
  b c |
  d g, |
  
  ees' ees \bar "||"
  ees2 |
  g,4( f) |
  ees2\fermata \bar ":|."
  ees'^\markup\italic{ "Versus Postremus"} |
  g4( f) |
  ees2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  bes4 d |
  ees ees |
  ees d |
  d ees \bar "||"
  c4. e8 |
  f4 ees |
  
  d d |
  d ees \bar "||"
  g g |
  g4. g8 |
  g4 a |
  g fis \bar "||"
  fis! g |
  
  a g |
  g fis |
  g g \bar "||"
  ees4. d8 |
  c4 c |
  f4. ees8 |
  ees4 d |
  
  ees ees |
  ees ees |
  ees e |
  f2 \bar "||"
  f4 f |
  f f |
  f f |
  
  ees aes \bar "||"
  g2 |
  ees4( d) |
  ees2 |
  g2 |
  bes4.( aes8) |
  g2 \bar "|."
}
altoWords = {
  \dropLyricsV
  \lyricmode {
    \set stanza = #"1. "
    A -- ve Je -- su De -- us ma -- gne,
    A -- ve Pu -- er, mi -- tis a -- gne,
    A -- ve De -- us ho -- mo na -- te,
    In Præ -- sé -- pi re -- cli -- ná -- te!
  }
  
  \set stanza = \markup\dynamic"f   "
  \lyricmode {
    \set associatedVoice = "tenors"
    O pot -- és -- tas, o e -- gés -- tas,
    O ma -- jés -- tas Dó -- mi -- ni!
    O ma -- jés -- tas, quid non præ -- stas
  }
  \set stanza = \markup\dynamic"  ff "
  \lyricmode{
    hó -- mi -- ni?
    hó -- mi -- ni?
  }
}
altoWordsII = \lyricmode {
  \dropLyricsV
%\markup\italic
  \set stanza = #"2. "
  Ut me páu -- pe -- rem di -- tá -- res,
  Ut me pér -- di -- tum sal -- vá -- res,
  Ja -- ces pan -- nis in -- vo -- lú -- tus,
  Om -- ni o -- pe de -- sti -- tú -- tus.
}
altoWordsIII = {
  \dropLyricsV
  \set stanza = \markup{\dynamic "mf  " "3. "}
  \lyricmode {
    In -- ter bru -- ta quam ab -- jéc -- tus
    Va -- gis, Pa -- tris o di -- léc -- tus!
    Ju -- dex sum -- me, ve -- rus De -- us,
    Prop -- ter me fis ho -- mo re -- us!
  }
}
altoWordsIV = \lyricmode {
  \dropLyricsV
  \set stanza = #"4. "
  O mi Je -- su, cor de -- vó -- tum
  Post te tra -- he, su -- me to -- tum,
  I -- gne tu -- o sanc -- to u -- re,
  Ah, ah pé -- ni -- tus com -- bú -- re.
}
altoWordsV = \lyricmode {
  \dropLyricsV
  \set stanza = #"5. "
  Pro -- cul va -- nos hinc a -- mó -- res,
  Pro -- cul ma -- los ar -- ce mo -- res,
  Tu -- is me -- os ap -- tos fin -- ge,
  Æ -- tér -- no me ne -- xu strin -- ge,
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  g4 aes |
  g aes |
  bes aes |
  aes g \bar "||"
  c4. bes8 |
  aes4 g |
  
  f bes |
  bes bes \bar "||"
  g a |
  bes4. bes8 |
  ees4 ees |
  d d \bar "||"
  c^\p d |
  
  ees4 ees |
  d4. c8 |
  bes4 bes \bar "||"
  bes4. bes8 |
  c4 c |
  c4. c8 |
  bes4 bes \bar "||"
  
  bes bes |
  c c |
  c c |
  c2 \bar "||"
  c4 c |
  d c |
  b b |
  
  c ees \bar "||"
  ees2 |
  bes4( aes) |
  g2 |
  
  ees' |
  ees4( d) |
  ees2 \bar "|."
  
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  ees4 ees |
  ees c |
  bes bes |
  c c \bar "||"
  aes'4. g8 |
  f4 aes, |
  
  bes4 bes |
  ees ees \bar "||"
  ees ees |
  bes4. bes8 |
  c4 c |
  d d \bar "||"
  a bes |
  
  c c |
  d d |
  g g \bar "||"
  ees4. ees8 |
  aes4 aes |
  f f |
  bes aes \bar "||"
  
  g g |
  c c |
  aes g |
  f2 \bar "||"
  aes4 aes |
  aes aes |
  g g |
  
  c c \bar "||"
  bes2 |
  bes, |
  ees\fermata |
  bes' |
  bes, |
  ees \bar "|."
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
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
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
     \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "altos" \altoWords
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

