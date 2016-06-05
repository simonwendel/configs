import {Gulpclass, Task, SequenceTask, } from "gulpclass/Decorators";

/* Configuration. */
const TSCONFIG = 'tsconfig.json';
const BASEDIR = './';
const APPDIR = './app/';

/* Global require should always be available at runtime. */
declare function require(name:string):any;

/* Gulp modules. */
const gulp = require("gulp");
const tsc = require('gulp-typescript');
const sourcemaps = require('gulp-sourcemaps');
const del = require('del');
const Server = require('karma').Server;

@Gulpclass()
export class Gulpfile {

    private tsProject = tsc.createProject(TSCONFIG);

    /**
     * COMPILE.
     * Builds TypeScript source files into the application directory.
     */
    @Task('compile', ['clean'])
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
    clean(cb:Function) {
        return del([
                APPDIR + '**/*.js',
                APPDIR + '**/*.js.map'],
            cb);
    }

    /**
     * TDD.
     * Runs the jasmine test suite in the karma runner, with file watches
     * and repeated runs on file changes.
     */
    @Task('tdd', ['compile'])
    tdd(cb:Function) {
        new Server({
            configFile: __dirname + '/karma.conf.js',
            autoWatch: true,
            singleRun: false
        }, cb).start();
    }

    /**
     * DEFAULT.
     * Runs TDD.
     */
    @SequenceTask()
    default() {
        return ['tdd'];
    }

}
