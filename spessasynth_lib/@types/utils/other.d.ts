/**
 * other.js
 * purpose: contains some useful functions that don't belong in any specific category
 */
/**
 * Formats the given seconds to nice readable time
 * @param totalSeconds {number} time in seconds
 * @return {{seconds: number, minutes: number, time: string}}
 */
export function formatTime(totalSeconds: number): {
    seconds: number;
    minutes: number;
    time: string;
};
/**
 * @param fileName {string}
 * @returns {string}
 */
export function formatTitle(fileName: string): string;
/**
 * Does what it says
 * @param arr {number[]}
 * @returns {string}
 */
export function arrayToHexString(arr: number[]): string;
export namespace consoleColors {
    let warn: string;
    let unrecognized: string;
    let info: string;
    let recognized: string;
    let value: string;
}
