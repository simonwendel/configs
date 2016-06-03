import {Gulpclass, Task} from "gulpclass/Decorators";

/* Configuration. */
const TSCONFIG = 'tsconfig.json';
const BASEDIR = './';
const APPDIR = './app/';

/* Global require should always be available at runtime. */
declare function require(name:string): any;

/* Gulp modules. */
const gulp = require("gulp");
const tsc = require('gulp-typescript');
const sourcemaps = require('gulp-sourcemaps');
const del = require('del');

@Gulpclass()
export class Gulpfile {

    private tsProject = tsc.createProject(TSCONFIG);

    /**
     * COMPILE.
     * Builds TypeScript source files into the application directory.
     */
    @Task()
    compile() {
        const tsResult = this.tsProject.src()
            .pipe(sourcemaps.init())
            .pipe(tsc(this.tsProject));

        return tsResult.js
            .pipe(sourcemaps.write(BASEDIR))
            .pipe(gulp.dest((file) => file.base));
    }

    /**
     * CLEAN.
     * Cleans compiled files from the application directory.
     */
    @Task()
    clean(cb: Function) {
        return del([
            APPDIR + '**/*.js',
            APPDIR + '**/*.js.map'],
            cb);
    }

}
