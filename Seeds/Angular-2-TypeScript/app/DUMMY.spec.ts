import { DummyField } from './DUMMY';

describe('TEST THAT THE RUNNER WORKS', () => {
    it('true is true', () => expect(true).toEqual(true));

    it('Imported DummyField has some value', () => expect(DummyField).toEqual('Some value'));
});
