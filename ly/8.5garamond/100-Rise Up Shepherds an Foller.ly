\version "2.24.0"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Rise Up, Shepherds, and Follow"}}
  poet = \markup\oldStyleNum"Traditional"
  composer = \markup\oldStyleNum"Spiritual"
  arranger = \markup\oldStyleNum"Arranged by Allen L. Richardson"
  tagline = ""
}
\paper {
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #100
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
  \time 4/4
  \override DynamicLineSpanner.staff-padding = #0.0
  \override DynamicLineSpanner.Y-extent = #'(-0.5 . 0.5)
}

sopMusic = \relative c'' {
  \partial 4 ees8\noBeam^\mf c |
  \slurDashed
  ees4 ees8\noBeam( c) ees4 c8\noBeam( c) |
  \slurSolid
  c4 bes bes2 |
  bes4^\p bes des8\noBeam des c4 |
  bes8 bes4. bes4\rest ees8\noBeam^\f c | \break
  
  \slurDashed
  ees4 c8\noBeam( c) ees4 c8\noBeam c |
  \slurSolid
  bes4 c g8[( f] ees4) |
  g4^\mf g f8\noBeam f ees4 |
  c8 ees4. bes'2\rest \bar "||" \break
  
  ees,4^\markup{\dynamic"f" \italic "più mosso"} g bes bes |
  c-> c-> bes2-> |
  bes4^\markup\italic"rit." bes des8\noBeam des c4 |
  bes8 bes4. bes2\rest |
  ees,4^\markup\italic"a tempo" g bes bes |
  
  c-> c-> bes2-> |
  g4^\markup\italic"rit." g f8\noBeam f ees4 |
  c8 ees4. bes'2\rest |
  ees,4.(^\ff^\<^\markup\italic"a tempo" g8) bes2 |
  bes bes |
  
  bes4\! bes des8\noBeam des^\> c4 |
  bes8\! bes4. bes2\rest |
  ees,4^\< g8.\noBeam g16 bes4 bes\! |
  c^\> bes g8[( f] ees4) |
  g\!^\markup{\dynamic "p" \italic"rit."} g f8\noBeam f ees4 |
  c8 ees4.\fermata bes'4\rest \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c'' {
  g8\noBeam aes |
  \slurDashed
  g4 g8\noBeam( aes) g4 g8\noBeam( g) |
  \slurSolid
  aes4 aes g2 |
  f4 f e8\noBeam e ees4 |
  d8 d4. s4 ees8\noBeam  f |
  
  \slurDashed
  g4 aes8\noBeam( aes) g4 f8\noBeam f |
  \slurSolid
  d4 d d( ees) |
  ees ees d8\noBeam  d c4 |
  c8 bes4. s2 \bar "||"
  
  ees4 g bes bes |
  c c bes2 |
  f4 f e8\noBeam e ees4 |
  d8 d4. s2 |
  ees4 g bes bes |
  
  c c bes2 |
  ees,4 ees d8\noBeam d c4 |
  c8 bes4. s2 |
  ees4.( g8) bes2 |
  bes bes |
  
  f4 f e8\noBeam e ees4 |
  d8 d4. s2 |
  ees4 g8.\noBeam g16 bes4 bes |
  c bes g8[( f] ees4) |
  ees4 ees d8\noBeam d c4 |
  c8 bes4. s4 \bar "|."
}
altoWords = \lyricmode {
  \dropLyricsIX
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  There’s a star in the East on _ Christ -- mas morn,
  Rise up, shep -- herds, and fol -- low;
  It -- ’ll lead to the place where the Sav -- ior’s born, _ _
  Rise up, shep -- herds, and fol -- low;
  
  Leave your ewes and leave your lambs,
  Rise up, shep -- herds, and fol -- low,
  Leave your sheep and leave your rams,
  Rise up, shep -- herds, and fol -- low.
  Fol -- _ low, fol -- low,
  Rise up, shep -- herds, and fol -- low;
  Fol -- low the star of Beth -- le -- hem, _ _
  Rise up, shep -- herds, and fol -- low.
}
altoWordsII = \lyricmode {
  \dropLyricsIX
%\markup\italic
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  If you take good _ heed to the An -- gels’ words,
  Rise up, shep -- herds, and fol -- low;
  You’ll for -- get your _ flocks, you’ll for -- get your herds, _ _
  Rise up, shep -- herds, and fol -- low.
}
altoWordsIII = \lyricmode {
}
altoWordsIV = \lyricmode {
}
altoWordsV = \lyricmode {
}
altoWordsVI = \lyricmode {
}
tenorMusic = \relative c' {
  bes8\noBeam_\mf c |
  \slurDashed
  bes4 bes8\noBeam( c) bes4 c8\noBeam( c) |
  \slurSolid
  ees4 d ees2 |
  des4_\p des bes8\noBeam bes a4 |
  aes!8 aes4. s4 g8\noBeam_\f aes |
  
  \slurDashed
  bes4 c8\noBeam( c) bes4 aes8\noBeam aes |
  \slurSolid
  bes4 aes aes( g) |
  a4_\mf a aes8\noBeam aes aes4 |
  aes8 g4. s2 \bar "||"
  
  ees4_\f g bes bes |
  c c bes2 |
  des4_\markup\italic"rit." des bes8\noBeam bes a4 |
  aes!8 aes4. s2 |
  ees4 g bes bes |
  
  c c bes2 |
  a4_\markup\italic"rit." a aes8\noBeam aes aes4 |
  aes8 g4. s2 |
  ees4.(_\ff_\< g8) bes2 |
  bes bes |
  
  des4\! des bes8\noBeam bes_\> a4 |
  aes!8\! aes4. s2 |
  ees4_\< g8.\noBeam g16 bes4 bes\! |
  c4_\> bes g8[( f] ees4) |
  bes'4_\p a aes8\noBeam aes aes4 |
  aes8 g4. s4 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
 ees8\noBeam ees |
 \slurDashed
 ees4 ees8\noBeam( ees) ees4 e8\noBeam(e) |
 \slurSolid
 f4 bes ees,2 |
 bes'4 aes g8\noBeam g fis4 |
 f!8 bes,4. d4\rest ees8\noBeam ees |
 
 \slurDashed
 ees4 ees8\noBeam( ees) ees4 ees8\noBeam ees |
 \slurSolid
 bes4 bes b( c) |
 f f bes,8\noBeam bes bes4 |
 ees8 ees4. d2\rest \bar "||"
 
 ees4 g bes bes |
 c-> c-> bes2-> |
 bes4 aes g8\noBeam g fis4 |
 f!8 bes,4. d2\rest |
 ees4 g bes bes |
 
 c-> c-> bes2-> |
 f4 f bes,8\noBeam bes bes4 |
 ees8 ees4. d2\rest |
 ees4.( g8) bes2 |
 bes bes |
 
 bes4 aes g8\noBeam g fis4 |
 f!8 bes,4. d2\rest |
 ees4 g8.\noBeam g16 bes4 bes |
 c bes g8[( f] ees4) |
 ees4 f bes,8\noBeam bes bes4 |
 ees8 ees4.\fermata d4\rest \bar "|."
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
    \new Lyrics \with { alignAboveContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((basic-distance . 1))} \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosVI"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsVI
    \new Lyrics = "altosV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsV
    \new Lyrics = "altosIV"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIV
    \new Lyrics = "altosIII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsIII
    \new Lyrics = "altosII"  \with { alignBelowContext = #"women" } \lyricsto "sopranos" \altoWordsII
    \new Lyrics = "altos"  \with { alignBelowContext = #"women" \override VerticalAxisGroup.nonstaff-relatedstaff-spacing = #'((padding . -0.5)) } \lyricsto "sopranos" \altoWords
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
%6.14 \context {\Lyrics\override LyricText.font-size = #0.8 }
%{IF_LESSER
\context {\Lyrics\override LyricText.font-size = #1.2 }
%}%END_IF_LESSER
    \context {
      \Score
      \override SpacingSpanner.base-shortest-duration = #(ly:make-moment 1/4)
      \override SpacingSpanner.common-shortest-duration = #(ly:make-moment 1/4)
    }
    \context {
      % a little smaller so lyrics
      % can be closer to the staff
      \Staff
      \override VerticalAxisGroup.minimum-Y-extent = #'(-3 . 3)
    }
    \context {
      \Lyrics
      \override LyricText.X-offset = #center-on-word
    }
  }
}

