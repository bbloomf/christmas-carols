/**
 * Enables or disables looging
 * @param enableInfo {boolean} - enables info
 * @param enableWarn {boolean} - enables warning
 * @param enableGroup {boolean} - enables groups
 * @param enableTable {boolean} - enables tables
 */
export function SpessaSynthLogging(enableInfo: boolean, enableWarn: boolean, enableGroup: boolean, enableTable: boolean): void;
/**
 * @param message {...any}
 */
export function SpessaSynthInfo(...message: any[]): void;
/**
 * @param message {...any}
 */
export function SpessaSynthWarn(...message: any[]): void;
export function SpessaSynthTable(...args: any[]): void;
/**
 * @param message {...any} the message
 */
export function SpessaSynthGroup(...message: any[]): void;
/**
 * @param message {...any} the message
 */
export function SpessaSynthGroupCollapsed(...message: any[]): void;
export function SpessaSynthGroupEnd(): void;
