/* Forma geometrica con metodo per calcolo area*/

enum FormaGeometrica {
    Cerchio(f32),
    Rettangolo(f32,f32)
}

impl FormaGeometrica {
    fn area(&self) -> f32 {
        match self {
            FormaGeometrica::Cerchio(radius) => radius*radius*3.14,
            FormaGeometrica::Rettangolo(b, h) => b*h
        }
    }
}

fn main() {
    let c = FormaGeometrica::Cerchio(2.0);
    let r = FormaGeometrica::Rettangolo(2.0, 5.0);

    let s_c = c.area();
    let s_r = r.area();

    println!("Area del cerchio: {s_c}");
    println!("Area del rettangolo: {s_r}");
}