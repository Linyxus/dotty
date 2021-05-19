trait FooBase {
  type This >: this.type <: FooBase { type This <: FooBase.this.This } & FooBase { type This <: FooBase.this.This }
}
