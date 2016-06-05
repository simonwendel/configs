import {Gulpclass, Task, SequenceTask} from 'gulpclass/Decorators';

/* Configuration. */
const TSCONFIG = 'tsconfig.json';
const BASEDIR = './';
const APPDIR = './app/';

/* Global require should always be available at runtime. */
declare function require(name: string): any;

/* Gulp modules and stuff. */
const gulp = require('gulp');
const plumber = require('gulp-plumber');
const del = require('del');
const tsc = require('gulp-typescript');
const sourcemaps = require('gulp-sourcemaps');
const Server = require('karma').Server;

@Gulpclass()
export class Gulpfile {

    private tsProject = tsc.createProject(TSCONFIG);

    /**
     * COMPILE.
     * Builds TypeScript source files into the application directory.
     */
    @Task('compile')
    compile() {
        const tsResult = this.tsProject.src()
            .pipe(plumber())
            .pipe(sourcemaps.init())
            .pipe(tsc(this.tsProject));

        return tsResult.js
            .pipe(sourcemaps.write(BASEDIR))
            .pipe(gulp.dest((file) => file.base));
    }

    /**
     * WATCH.
     * Watches TypeScript files in the application directory and compiles them
     * whenever they change.
     */
    @Task()
    watch(done: Function) {
        gulp.watch([APPDIR + '**/*.ts'], ['compile']);
        done();
    }

    /**
     * CLEAN.
     * Cleans compiled files from the application directory.
     */
    @Task()
    clean(done: Function) {
        return del([
                APPDIR + '**/*.js',
                APPDIR + '**/*.js.map'],
            done);
    }

    /**
     * TDD.
     * Runs the jasmine test suite in the karma runner, with file watches
     * and repeated runs on file changes.
     */
    @Task('tdd', ['compile'])
    tdd(done: Function) {
        new Server({
            configFile: __dirname + '/karma.conf.js',
            autoWatch: true,
            singleRun: false
        }, done).start();
    }

    /**
     * DEFAULT.
     * Runs TDD.
     */
    @SequenceTask()
    default() {
        return ['clean', 'watch', 'tdd'];
    }

}
